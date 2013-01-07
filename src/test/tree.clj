(ns test.tree
  (:require [clojure.zip :as zip]
            [clojure.pprint :as pprint]
            [slingshot.slingshot :refer [try+]]
            [clojure.core.incubator :refer [-?>]]
            (test.tree [zip :as tz]
                       [builder :as build]
                       [reporter :as reporter]))
  (:import (java.util.concurrent Executors ExecutorService Callable ThreadFactory
                                TimeUnit LinkedBlockingQueue ThreadPoolExecutor )))

(defn execute "Executes test, returns either :pass if the test exits
               normally, or exception the test threw."
  [{:keys [steps test]}]
  ;; if the request has a :steps entry, use that. if not, use :steps
  ;; entry from the :test map.
  (let [steps (or steps (:steps test))]
    (try+ {:returned (steps)            ;test fn is called here
           :result :pass}
          (catch Object _ {:result :fail
                           :error &throw-context}))))


(defn wrap-data-driven [runner]
  (fn [{{:keys [steps parameters]} :test :as req}]
    (if parameters
      (let [realized-params (build/realize parameters)]
        (-> req
           (assoc :steps (with-meta (fn [] (apply steps realized-params))
                           (meta steps)))
           runner
           (assoc :parameters realized-params)))
      (runner req))))

(defn wrap-blockers [runner]
  (fn [{{:keys [always-run]} :test blocked-by :blocked-by :as req}]
    (let [blocked? (-> blocked-by (or []) count (> 0))]
      (if (and blocked? (not always-run))
        {:result :skip
         :blocked-by blocked-by}
        (runner req)))))

(defn wrap-timer [runner]
  (fn [req]
    (let [start-time (System/currentTimeMillis)]
      (-> req
         runner
         (assoc :start-time start-time :end-time (System/currentTimeMillis))))))

(defn wrap-thread-logging [runner]
  (fn [req]
    (-> req runner (assoc :thread (.getName (Thread/currentThread))))))

(def ^{:dynamic true
       :doc "A rebindable test runner to allow extensibility. The
             runner will be a function of one arg - a request map
             like:
                 {:blocked-by ['myblocker']
                  :test { test data here ... } and returns a map of
             results. See test.tree.reports for contracts that the
             results should adhere to."}
  runner
  (-> execute
     wrap-blockers
     wrap-timer
     wrap-thread-logging
     wrap-data-driven))

(defn parent-blocker "Returns a list of parent nodes blocking this
                      test (since each node only has one parent, it
                      will either be empty or have 1 item, the parent
                      test)"
  [test-tree-zip reports]
  (let [parent (-?> test-tree-zip zip/up zip/node tz/plain-node)]
    (if (and parent (not (reporter/test-passed? reports parent)))
      [parent]
      [])))

(declare queue)

(defn run-test [test-tree-zip testrun-queue reports blockers]
  (let [this-test (-> test-tree-zip zip/node tz/plain-node)]
    (try (let [all-blockers (concat blockers (parent-blocker test-tree-zip reports))]
           (dosync
            (alter reports update-in [this-test]
                   assoc :status :running))
           (let [report (runner {:test this-test :blocked-by all-blockers})]
             (dosync
              (alter reports update-in [this-test]
                     merge {:status :done
                            :report report}))
             (deliver (:promise (@reports this-test)) :done)))
         (catch Exception e
           (deliver (:promise (@reports this-test)) e)
           (println "report delivered with error: "  (:name this-test) ": " e)
           (.printStackTrace e))))
  (doseq [child-test (tz/child-locs test-tree-zip)]
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
    (let [blockers (try (doall ((or (-> test-tree-zip zip/node :blockers) ;; TODO is doall really needed?
                                    (constantly [])) ;;default blocker fn returns empty list
                                test-tree-zip))
                        (catch Exception e [e]))]
      (.offer testrun-queue (fn [] (run-test test-tree-zip testrun-queue reports blockers)))
      (dosync
       (alter reports assoc-in [(-> test-tree-zip zip/node tz/plain-node) :status] :queued)))))

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
         (when (live? t) (.stop t))))

(defn wait-for-all-test-results [threads reports]
 (loop [s (state threads reports)]
    (case s
      :finished nil
      :deadlocked (throw (RuntimeException. "All threads died with tests still in the queue! aborting."))
      :running (do (Thread/sleep 200)
                   (recur (state threads reports)))))) 
(defn run
  "Runs all tests in the tree, in parallel (if threads are set >1).
   Returns a two-element list containing the worker threads and test 
   reports data."
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
        test-tree-zip (tz/test-zip tree)
        reports (reporter/init-reports test-tree-zip)]

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

;; save the original core dispatch functions
(defonce print-method-orig-object (get-method print-method Object))
(defonce print-method-orig-ideref (get-method print-method clojure.lang.IDeref))

(defn print-quoted [f o w]
  (.write w "\"")
  (f o w)
  (.write w "\""))


(defmethod print-method clojure.lang.IDeref [o ^java.io.Writer w]
  (if *print-all-readably*
    (print-quoted print-method-orig-ideref o w)
    (print-method-orig-ideref o w)))

(defmethod print-method Object [o, ^java.io.Writer w]
  (if *print-all-readably*
    (print-quoted print-method-orig-object o w)
    (print-method-orig-object o w)))

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
              (pr (list tree @reports)))))
    (binding [*print-all-readably* true]
      (redir [*out* (java.io.FileWriter. "testng-report.xml")]
             (reporter/testng-report @reports))
      (redir [*out* (java.io.FileWriter. "junitreport.xml")]
             (reporter/junit-report @reports)))
    @reports))
