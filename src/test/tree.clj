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

(defprotocol Blocker
  (blockers [x m]
    "If x blocks the current test, returns a list of blockers, otherwise empty list.
     m is a map of the state for the current test:

      {:test-zipper #<A zipper structure, with the 'current' location
                      set to the test being run>
       :reports #<A ref to the reports of the entire test run>}

     Sometimes you need access to this information to determine
     whether a test should be blocked or not.  For example, it may
     depend on another test that is not the direct parent.  If you
     don't need any of this information, you can just ignore the
     argument."))

;; default implementation (always a blocker, returns list of one item))
(extend java.lang.Object
  Blocker {:blocks list})

;; treat functions as being a function of test state
;; any other info needs to be closed over.  Function
;; should return a list of blocking objects
(extend-type clojure.lang.AFn
  Blocker (blockers [f m]
            (f m)))

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
      ;;only list the name and parameters of a blocking test
      [(reporter/blocking-test parent)]
      [])))

(declare queue)

(defn run-test [test-tree-zip testrun-queue reports test-blockers]
  (let [this-test (-> test-tree-zip zip/node tz/plain-node)]
    (try (let [all-blockers (concat test-blockers (parent-blocker test-tree-zip reports))]
           (dosync
            (alter reports assoc-in [this-test :status] :running))
           (let [report (runner {:test this-test :blocked-by all-blockers})]
             (dosync
              (deliver (-> reports deref (get this-test) :lock) :done)
              (alter reports update-in [this-test] merge {:status :done, :report report}))))
         (catch Exception e
           (deliver (:lock (@reports this-test)) e)
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
    ;; force calculation of blockers so that exceptions will be caught
    ;; here, rather than possibly uncaught where the values are consumed.
    (let [test-blockers (try (doall
                              (mapcat #(blockers % {:test-zipper test-tree-zip
                                                    :reports reports})
                                      (-> test-tree-zip zip/node :blockers)))
                             (catch Exception e [e]))]
      (.offer testrun-queue (fn [] (run-test test-tree-zip testrun-queue reports test-blockers)))
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
        unrun-tests (some (complement realized?) (map :lock (vals @reports)))]
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
                     (while (= (state thread-pool reports) :running)
                       (Thread/sleep 250))
                                        ;signal the threads to stop waiting for new
                                        ;tests
                     (reset! testrun-state :finished)
                     (teardown)
                     reports)]
      (setup)

      (queue test-tree-zip testrun-queue reports)
      [thread-pool reports])))

(defmacro redir-out-to-file [file & body]
  `(with-open [fw# (java.io.FileWriter. ~file)]
     (binding [*out* fw#]
       ~@body)))


;; Depending on the value of this var, either print the standard
;; clojure way, or turn all objects that can't be printed readably
;; into strings that can at least be read in.

(def ^:dynamic *print-all-readably* nil)

;; save the original core dispatch functions
(defonce print-method-orig-object (get-method print-method Object))
(defonce print-method-orig-ideref (get-method print-method clojure.lang.IDeref))

(defn print-quoted [f o w]
  (clojure.lang.RT/print (str "#<" o ">") w))

(defn print-ideref [f o w]
  (clojure.lang.RT/print (str
                          (if (and (instance? clojure.lang.IPending o) (not (.isRealized o)))
                                            :pending
                                            @o)) w))

(defmethod print-method clojure.lang.IDeref [o ^java.io.Writer w]
  (if *print-all-readably*
    (print-ideref print-method-orig-ideref o w)
    (print-method-orig-ideref o w)))

(defmethod print-method Object [o, ^java.io.Writer w]
  (if *print-all-readably*
    (print-quoted print-method-orig-object o w)
    (print-method-orig-object o w)))

;; Print clojure records with #Foopkg.fooclass{:a "bar"} style
(defmethod clojure.pprint/simple-dispatch clojure.lang.IRecord [obj] (pr obj))
(prefer-method clojure.pprint/simple-dispatch clojure.lang.IRecord clojure.lang.IPersistentMap)

(defn run-suite "Run the test tree (blocking until all tests are
                 complete) and return the reports list.  Also writes a
                 junit report file to the current directory, and a
                 clojure data file with all the results.  If you want
                 to just run the tests without blocking, see 'run'."
  [tree & [opts]]
  (let [[threads reports] (run tree opts)]
    (wait-for-all-test-results threads reports)
    (binding [*print-all-readably* true]
      (redir-out-to-file "report.clj"
                         (binding [*print-length* nil
                                   *print-level* nil]
                           (pr (list tree @reports))))
      (spit "testng-report.xml" (reporter/testng-report @reports))
      (spit "junitreport.xml" (reporter/junit-report @reports)))
    @reports))
