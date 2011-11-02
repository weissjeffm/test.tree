(ns test.tree
  (:require [clojure.zip :as zip]
            [clojure.pprint :as pprint]
            
            )
  (:use [clojure.core.incubator :only [-?>]])
  
  (import (java.util.concurrent Executors ExecutorService Callable ThreadFactory
                                TimeUnit LinkedBlockingQueue ThreadPoolExecutor )))

(def q (atom nil))
(def reports (ref {}))
(def done (atom nil))

;;
;;post-execution reporting functions
;;
;;test execution functions



(defn execute "Executes test, calls listeners, returns either :pass
                    if the test exits normally,
                    :skip if a dependency failed, or an exception the test threw." 
  [test]    
  (let [start-time  (System/currentTimeMillis)]
    (merge (try {:returned ((:steps test)) ;test fn is called here
                 :result :pass}
                (catch Throwable t {:result t}))
           {:start-time start-time
            :end-time (System/currentTimeMillis)})))

(declare queue)

(defn parent-blocker [z]
  (let [parent (-?> z zip/up zip/node plain-node)]
    (if (and parent
             (not (passed? parent)))
      [parent]
      [])))
    
(defn run-test [z blockers]
  (comment (println (str "running test: " (:name (zip/node z))) ))
  (let [this-test (-> z zip/node plain-node)]
    (try (let [all-blockers (concat blockers (parent-blocker z))
               blocked? (-> all-blockers count (> 0))
               report (merge {:thread (.getName (Thread/currentThread))}
                             (if (or (:always-run this-test)
                                     (not blocked?))
                               (execute (plain-node this-test))
                               (let [timestamp (System/currentTimeMillis)]
                                 (merge {:result :skip
                                         :start-time timestamp
                                         :end-time timestamp}
                                        (if blocked?
                                          {:blocked-by all-blockers} {})))))]
           (dosync
            (alter reports update-in [this-test] merge {:status :done
                                                        :report report}))

           (comment (println "Adding report" (@reports this-test)))
           (deliver (:promise (@reports this-test))
                    :done)
           (comment (println "report delivered: "  (:name this-test) ": "
                     (dissoc report :start-time :end-time))))
         (catch Exception e
           (deliver (:promise (@reports this-test))
                    e)
           (println "report delivered with error: "  (:name this-test) ": " e))))
  (doseq [child-test (child-locs z)]
    (queue child-test)))

(defn consume "Starts polling the test queue, takes tests from the
               queue and executes them one at a time."
  []
  (while (and @q
              (not (and @done
                        (.isEmpty @q))))
    (if-let [next-item (.poll @q (long 500) TimeUnit/MILLISECONDS)]
      (next-item)))
  (if-not @q (println "queue reset, thread exiting.")
          (println "thread done.")))

(defn queue "In a future, waits for the calculation of blockers for
             the current test, and puts it on the queue.  Even if the
             test turns out to be blocked, it won't be marked skipped
             until it is consumed."
  [z]
  (future
    (let [blockers (try 
                       ((or (-> z zip/node :blockers)
                            (constantly []))  ;;default blocker fn returns empty list
                         z)
                       (catch Exception e [e]))]  
      (comment (println (str "queueing: " (-> z zip/node :name))))
      (.offer @q (fn [] (run-test z blockers)))
      (dosync
       (alter reports
              assoc-in [(-> z zip/node plain-node) :status] :queued)))))

(defn run-allp [tree]
  (let [thread-runner (or (-> tree meta :thread-runner) identity)
        setup (or (-> tree meta :setup) (constantly nil))
        teardown (or (-> tree meta :teardown) (constantly nil))
        numthreads (or (-> tree meta :threads) 1)
        watchers (or (-> tree meta :watchers) {})
        z (test-zip tree)] 
    
    (reset! q (LinkedBlockingQueue.))
    (reset! done false)

    ;;initialize reports
    (dosync (ref-set reports
                     (zipmap (nodes z)
                             (repeatedly (fn [] {:status :waiting
                                                :promise (promise)})))))
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

(defn run-suite "Run the test tree (blocking until all tests are
                 complete) and return the reports list.  Also writes a
                 junit report file to the current directory, and a
                 clojure data file with all the results.  If you want
                 to just run the tests without blocking, use run-allp."
                 [tree] @(run-allp tree)
  (spit "junitreport.xml" (junit-report))
  (spit "report.clj" (with-out-str
                       (binding [pprint/*print-right-margin* 120
                                 pprint/*print-suppress-namespaces* true
                                 pprint/*print-miser-width* 80]
                         (pprint/pprint (sort-by (fn [item] (-> item :report :start-time))
                                                 (map #(assoc %1 :report %2)
                                                      (keys @reports)
                                                      (vals @reports)))))))
  @reports)


