(ns test.tree.execute
  (:require [clojure.zip :as zip]
            [slingshot.slingshot :refer [try+]]
            [clojure.core.incubator :refer [-?>]]
            [test.tree :as tree]
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
           reporter/outcome :pass}
          (catch Object _ {reporter/outcome :fail
                           :reporter/error &throw-context}))))


(defn wrap-data-driven [runner]
  (fn [{{:keys [steps parameters]} :test :as req}]
    (if parameters
      (let [realized-params (build/realize parameters)]
        (-> req
           (assoc :steps (with-meta (fn [] (apply steps realized-params))
                           (meta steps)))
           runner
           (assoc :reporter/parameters realized-params)))
      (runner req))))

(defn wrap-blockers [runner]
  (fn [{{:keys [always-run]} :test blocked-by :blocked-by :as req}]
    (let [blocked? (-> blocked-by (or []) count (> 0))]
      (if (and blocked? (not always-run))
        {reporter/outcome :skip
         :reporter/blocked-by blocked-by}
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
                 {:reporter/blocked-by ['myblocker']
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
      ;;only list the name and parameters of a blocking test
      [(reporter/blocking-test parent)]
      [])))

(declare queue)

(defn run-test [test-tree-zip testrun-queue reports blockers]
  (let [this-test (-> test-tree-zip zip/node tz/plain-node)]
    (try (let [all-blockers (concat blockers (parent-blocker test-tree-zip reports))]
           (dosync (commute reports update-in [this-test]
                            assoc reporter/status :running))
           (let [result (try
                          (runner {:test this-test :reporter/blocked-by all-blockers})
                          (catch Exception e
                            (println "result delivered with error: "  (:name this-test) ": " e)
                            (.printStackTrace e)
                            e))]
             (dosync (commute reports update-in [this-test] assoc
                              reporter/status :done,
                              reporter/result result))))))
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
    ;; force calculation of blockers so that exceptions will be caught
    ;; here, rather than possibly uncaught where the values are consumed.
    (let [blockers (try (doall ((or (-> test-tree-zip zip/node :blockers) 
                                    (constantly [])) ;;default blocker fn returns empty list
                                {:test-zipper test-tree-zip
                                 :reports reports}))
                        (catch Exception e [e]))]
      (.offer testrun-queue (fn [] (run-test test-tree-zip testrun-queue reports blockers)))
      (dosync
       (alter reports assoc-in [(-> test-tree-zip zip/node tz/plain-node) reporter/status] :queued)))))

(defn live? "is the thread alive (not terminated?)"
  [t]
  (not= Thread$State/TERMINATED (.getState t)))

(defn state
  "If some threads are still live and some promises
   not yet delivered, tests are still running."
  [threads reports]
  (let [living-threads (some live? threads)
        unrun-tests (some (complement (partial = :done)) (map reporter/status (vals @reports)))]
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
   Returns a two-element list containing the worker threads and ref to
   test reports data."
  [tree & [{:keys [thread-runner setup teardown
                   threads watchers reports-ref]
            :or {thread-runner identity
                 setup (constantly nil)
                 teardown (constantly nil)
                 threads 1
                 watchers {}}}]]
  (let [testrun-queue (LinkedBlockingQueue.)
        testrun-state (atom :not-started)
        thread-pool (for [agentnum (range threads)]
                      (Thread. (-> (partial consume testrun-queue testrun-state)
                                  thread-runner)
                               (str "test.tree-thread" agentnum)))
        test-tree-zip (tz/test-zip tree)
        reports (let [empty-reports (reporter/init-reports test-tree-zip)]
                  (if reports-ref
                    (dosync (ref-set reports-ref empty-reports) reports-ref)
                    (ref empty-reports)))]

    ;;start worker threads
    (doseq [thread thread-pool] (.start thread))
    
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
      [thread-pool reports])))

(defmacro redir [[v stream] & body]
  `(binding [~v ~stream]
     (try ~@body
          (finally (.close ~v)))))

(defn run-suite "Run the test tree (blocking until all tests are
                 complete) and return the reports list.  Also writes a
                 junit result file to the current directory, and a
                 clojure data file with all the results.  If you want
                 to just run the tests without blocking, see 'run'."
  [tree & [opts]]
  (let [[threads reports] (run tree opts)]
    (wait-for-all-test-results threads reports)
    (binding [tree/*print-all-readably* true]
      (spit "report.clj"
            (with-out-str
              (binding [*print-length* nil
                        *print-level* nil]
                (pr (list tree @reports)))))
      (redir [*out* (java.io.FileWriter. "testng-report.xml")]
             (reporter/testng-report @reports))
      (redir [*out* (java.io.FileWriter. "junitreport.xml")]
             (reporter/junit-report @reports)))
    @reports))