(ns test.tree
  (:require [clojure.zip :as zip]
            [clojure.pprint :as pprint])
  (:use slingshot.slingshot
        [clojure.core.incubator :only [-?>]]
        test.tree.zip
        [test.tree.builder :only [realize]]
        [test.tree.reporter :only [passed? reports init-reports junit-report testng-report]])
  
  (import (java.util.concurrent Executors ExecutorService Callable ThreadFactory
                                TimeUnit LinkedBlockingQueue ThreadPoolExecutor )))


(def q (atom nil))
(def done (atom nil))

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
  [z]
  (let [parent (-?> z zip/up zip/node plain-node)]
    (if (and parent (not (passed? parent)))
      [parent]
      [])))

(declare queue)

(defn run-test [z blockers]
  (let [this-test (-> z zip/node plain-node)]
    (try (let [all-blockers (concat blockers (parent-blocker z)) 
               report (runner (assoc this-test :blocked-by all-blockers))]
           (dosync
            (alter reports update-in [this-test]
                   merge {:status :done
                          :report report}))
           (deliver (:promise (@reports this-test)) :done))
         (catch Exception e
           (deliver (:promise (@reports this-test)) e)
           (println "report delivered with error: "  (:name this-test) ": " e))))
  (doseq [child-test (child-locs z)]
    (queue child-test)))

(defn consume "Starts polling the test queue, takes tests from the
               queue and executes them one at a time."
  []
  (while (and @q (not (and @done (.isEmpty @q))))
    (if-let [next-item (.poll @q (long 100) TimeUnit/MILLISECONDS)]
      (next-item)))
  (if-not @q (println "queue reset, thread exiting.")
          (println "thread done.")))

(defn queue "In a future, waits for the calculation of blockers for
             the current test, and puts it on the queue.  Even if the
             test turns out to be blocked, it won't be marked skipped
             until it is consumed."
  [z]
  (future
    (let [blockers (try (doall ((or (-> z zip/node :blockers)
                                    (constantly [])) ;;default blocker fn returns empty list
                                z))
                        (catch Exception e [e]))]  
      (.offer @q (fn [] (run-test z blockers)))
      (dosync
       (alter reports assoc-in [(-> z zip/node plain-node) :status] :queued)))))

(defn run-allp "Runs all tests in the tree, in parallel (if threads
                are set >1).  Returns a future object that when
                deref'd will block until all tests are done, and
                return reports data."
  [tree]
  (let [thread-runner (or (-> tree meta :thread-runner) identity)
        setup (or (-> tree meta :setup) (constantly nil))
        teardown (or (-> tree meta :teardown) (constantly nil))
        numthreads (or (-> tree meta :threads) 1)
        watchers (or (-> tree meta :watchers) {})
        z (test-zip tree)] 
    
    (reset! q (LinkedBlockingQueue.))
    (reset! done false)

    ;;initialize reports
    (init-reports z)
    
    ;;watch reports
    (doseq [[k v] watchers]
      (add-watch reports k v))
    
    (let [end-wait (future ;;; when all reports are done, raise 'done' flag
                           ;;; and do teardown
                     (doseq [v (vals @reports)]
                       (-> v :promise deref))
                     (reset! done true) 
                     (teardown)
                     @reports)]
      (setup)
      (doseq [agentnum (range numthreads)]
        (.start (Thread. (-> consume
                            thread-runner)
                         (str "test.tree-thread" agentnum))))
      (queue z)
      end-wait)))

(defmacro redir [[v stream] & body]
  `(binding [~v ~stream]
     (try ~@body
          (finally (.close ~v)))))

(defn run-suite "Run the test tree (blocking until all tests are
                 complete) and return the reports list.  Also writes a
                 junit report file to the current directory, and a
                 clojure data file with all the results.  If you want
                 to just run the tests without blocking, use run-allp."
  [tree]
  @(run-allp tree)
  (redir [*out* (java.io.FileWriter. "junitreport.xml")]
    (junit-report))
  (redir [*out* (java.io.FileWriter. "testng-report.xml")]
    (testng-report))
  (spit "report.clj"
        (with-out-str
          (binding [pprint/*print-right-margin* 120
                    pprint/*print-suppress-namespaces* true
                    pprint/*print-miser-width* 80]
            (pprint/pprint (sort-by (fn [item] (-> item :report :start-time))
                                    (map merge
                                         (keys @reports)
                                         (for [v (vals @reports)]
                                           (dissoc v :promise :status))))))))
  @reports)


