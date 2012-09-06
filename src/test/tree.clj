(ns test.tree
  (:require [clojure.zip :as zip]
            [clojure.pprint :as pprint])
  (:use slingshot.slingshot
        [clojure.core.incubator :only [-?>]]
        test.tree.zip
        [test.tree.builder :only [realize]]
        [test.tree.reporter :only [passed? *reports* init-reports junit-report testng-report]])

  (import (java.util.concurrent Executors ExecutorService Callable ThreadFactory
                                TimeUnit LinkedBlockingQueue ThreadPoolExecutor )))

(defn execute "Executes test, returns either :pass if the test exits
               normally, or exception the test threw."
  [test]
  (try+ {:returned ((:steps test))      ;test fn is called here
         :result :pass}
        (catch Object _ {:result :fail
                         :error &throw-context})))


(defn wrap-data-driven [runner]
  (fn [{:keys [steps parameters] :as test}]
    (if parameters
      (let [realized-params (realize parameters)]
        (-> test
           (assoc :steps (with-meta (fn [] (apply steps realized-params))
                           (meta steps)))
           runner
           (assoc :parameters realized-params)))
      (runner test))))

(defn wrap-blockers [runner]
  (fn [{:keys [blocked-by always-run] :as test}]
    (let [blocked? (-> blocked-by (or []) count (> 0))]
      (if (and blocked? (not always-run))
        {:result :skip
         :blocked-by blocked-by}
        (runner test)))))

(defn wrap-timer [runner]
  (fn [test]
    (let [start-time (System/currentTimeMillis)]
      (-> test
         runner
         (assoc :start-time start-time :end-time (System/currentTimeMillis))))))

(defn wrap-thread-logging [runner]
  (fn [test]
    (-> test runner (assoc :thread (.getName (Thread/currentThread))))))

(def ^:dynamic runner
  (-> execute
     wrap-blockers
     wrap-timer
     wrap-thread-logging
     wrap-data-driven))

(defn parent-blocker "Returns a list of parent nodes blocking this
                      test (since each node only has one parent, it
                      will either be empty or have 1 item, the parent
                      test)"
  [test-tree-zip]
  (let [parent (-?> test-tree-zip zip/up zip/node plain-node)]
    (if (and parent (not (passed? parent)))
      [parent]
      [])))

(declare queue)

(defn run-test [test-tree-zip testrun-queue reports blockers]
  (let [this-test (-> test-tree-zip zip/node plain-node)]
    (try (let [all-blockers (concat blockers (binding [*reports* reports]
                                               (parent-blocker test-tree-zip)))
               report (runner (assoc this-test :blocked-by all-blockers))]
           (dosync
            (alter reports update-in [this-test]
                   merge {:status :done
                          :report report}))
           (deliver (:promise (@reports this-test)) :done))
         (catch Exception e
           (deliver (:promise (@reports this-test)) e)
           (println "report delivered with error: "  (:name this-test) ": " e)
           (.printStackTrace e))))
  (doseq [child-test (child-locs test-tree-zip)]
    (queue child-test testrun-queue reports)))

(defn consume "Starts polling the test queue, takes tests from the
               queue and executes them one at a time."
  [q testrun-state]
  (while (not= @testrun-state :finished)
    (if-let [next-item (.poll q (long 250) TimeUnit/MILLISECONDS)]
      (next-item)))
  (println "thread done."))

(defn queue "In a future, waits for the calculation of blockers for
             the current test, and puts it on the queue.  Even if the
             test turns out to be blocked, it won't be marked skipped
             until it is consumed."
  [test-tree-zip testrun-queue reports]
  (future
    (let [blockers (try (binding [*reports* reports]
                          (doall ((or (-> test-tree-zip zip/node :blockers)
                                      (constantly [])) ;;default blocker fn returns empty list
                                  test-tree-zip)))
                        (catch Exception e [e]))]
      (.offer testrun-queue (fn [] (run-test test-tree-zip testrun-queue reports blockers)))
      (dosync
       (alter reports assoc-in [(-> test-tree-zip zip/node plain-node) :status] :queued)))))

(defn live? "is the thread alive (not terminated?)"
  [t]
  (not= Thread$State/TERMINATED (.getState t)))

(defn state
  "If some threads are still live and some promises
   not yet delivered, tests are still running."
  [threads reports]
  (let [living-threads (some live? threads)
        unrun-tests (some (complement realized?) (map :promise (vals @reports)))]
    (cond (and living-threads unrun-tests) :running
          (not unrun-tests) :finished
          :else :deadlocked)))

(defn terminate-all-tests [threads]
  "Terminates all running test threads."
  (doseq [t threads] 
         (when (live?) (.terminate t))))

(defn wait-for-all-test-results [threads reports]
 (loop [s (state threads reports)]
    (case s
      :finished nil
      :deadlocked (throw (RuntimeException. "All threads died with tests still in the queue! aborting."))
      :running (do (Thread/sleep 200)
                   (recur (state threads reports)))))) 
(defn run
  "Runs all tests in the tree, in parallel (if threads are set >1).
  Returns a future object that when deref'd will block until all tests
  are done, and return reports data."
  [tree & [testrun-name]]
  (let [thread-runner (or (-> tree meta :thread-runner) identity)
        setup (or (-> tree meta :setup) (constantly nil))
        teardown (or (-> tree meta :teardown) (constantly nil))
        numthreads (or (-> tree meta :threads) 1)
        testrun-queue (LinkedBlockingQueue.)
        testrun-state (atom :not-started)
        threads (for [agentnum (range numthreads)]
                  (Thread. (-> (partial consume testrun-queue testrun-state)
                              thread-runner)
                           (str "test.tree-thread" agentnum)))
        watchers (or (-> tree meta :watchers) {})
        test-tree-zip (test-zip tree)
        reports (init-reports test-tree-zip)]

    ;;start worker threads
    (doseq [thread threads] (.start thread))
    
    ;;watch reports
    (doseq [[k v] watchers]
      (add-watch reports k v))

    (let [end-wait (future ;when all reports are done, raise 'done' flag
                           ;and do teardown
                     (while (= (state threads reports) :running)
                       (Thread/sleep 250))
                     ;signal the threads to stop waiting for new
                     ;tests
                     (reset! testrun-state :finished)
                     (teardown)
                     reports)]
      (setup)

      (queue test-tree-zip testrun-queue reports)
      [threads reports])))

(defmacro redir [[v stream] & body]
  `(binding [~v ~stream]
     (try ~@body
          (finally (.close ~v)))))


;; Depending on the value of this var, either print the standard
;; clojure way, or turn all objects that can't be printed readably
;; into strings that can at least be read in.

(def ^:dynamic *print-all-readably* nil)

(defmethod print-method Object [o, ^java.io.Writer w]
  (when *print-all-readably*
    (.write w "\""))
  (.write w "<")
  (.write w (.getSimpleName (class o)))
  (.write w " ")
  (.write w (str o))
  (.write w ">")
  (when *print-all-readably*
    (.write w "\"")))

(defn print-readable-report
  "Prints a report file in clojure data format, that can be read back
   in later and analyed."
  [data]

  )

(defn run-suite "Run the test tree (blocking until all tests are
                 complete) and return the reports list.  Also writes a
                 junit report file to the current directory, and a
                 clojure data file with all the results.  If you want
                 to just run the tests without blocking, see 'run'."
  [tree & [testrun-name]]
  (let [[threads reports] (run tree testrun-name)]
    (wait-for-all-test-results threads reports)
    (spit "report.clj"
          (with-out-str
            (binding [pprint/*print-right-margin* 120
                      pprint/*print-suppress-namespaces* true
                      pprint/*print-miser-width* 80
                      *print-length* nil
                      *print-level* nil
                      *print-all-readably* true]
              (pprint/pprint (sort-by (fn [item] (-> item :report :start-time))
                                      (map merge
                                           (keys @reports)
                                           (for [v (vals @reports)]
                                             (dissoc v :promise :status))))))))
    (binding [*reports* reports
              *print-all-readably* true]
      (redir [*out* (java.io.FileWriter. "testng-report.xml")]
             (testng-report))
      (redir [*out* (java.io.FileWriter. "junitreport.xml")]
             (junit-report)))

    @reports))
