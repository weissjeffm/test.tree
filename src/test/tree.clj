(ns test.tree
  (:require [clojure.zip :as zip]
            [clojure.pprint :as pprint]
            [clojure.stacktrace :as st]
            [clojure.contrib.prxml :as xml])
  (:use [clojure.contrib.core :only [-?>]]
        [pretzel.combine :only [every-p?]])
  (:refer-clojure :exclude [fn])
  (import (java.util.concurrent Executors ExecutorService Callable ThreadFactory
                                TimeUnit LinkedBlockingQueue ThreadPoolExecutor )))

(def q (atom nil))
(def reports (atom {}))
(def done (atom nil))

(defn print-meta [val]
  {:type ::serializable-fn
   ::source val})

(defmacro ^{:doc (str (:doc (meta #'clojure.core/fn))
                              "\n\n  Oh, but it also allows serialization!!!111eleven")}
          fn [& sigs]
          `(with-meta (clojure.core/fn ~@sigs)
             (print-meta (quote ~&form))))


(defmethod print-method ::serializable-fn [o ^Writer w]
  (print-method (::source (meta o)) w))

;;
;;pre-execution test manipulation functions
;;
(declare passed?)

(defn test-zip [tree] (zip/zipper (constantly true)
                                  :more 
                                  (fn [node children]
                                    (with-meta (conj node {:more children}) (meta node)))
                                    tree))

(defn walk-all "Does a depth-first walk of the tree, passes each node thru f, and returns the tree"
  [f tree]
  (let [walk-fn (fn [l]
                  (let [new-l (zip/edit l f)] 
                    (zip/next new-l)))]
    (->> tree
         test-zip
         (iterate walk-fn)
         (drop-while (complement zip/end?))
         first
         zip/root)))

(defn walk-all-matching [pred f tree]
  (walk-all (fn [n] ((if (pred n) f
                        identity) n))
            tree))

(defn plain-node [m]
  (dissoc m :more))

(defn child-locs [z]
  (let [is-child? (fn [loc] (some #{(and loc (zip/node loc))} 
                                 (zip/children z)))]
    (->> z
         zip/down
         (iterate zip/right)
         (take-while is-child?))))

(defn data-driven "Generate a set of n data-driven tests from a template
                   test, a function f that takes p arguments, and a n by p coll
                   of colls containing the data for the tests."
  [test f data]
  (for [item data] (merge (or (meta data) {})
                          (assoc test
                            :steps (with-meta (apply partial f item) (meta f))
                            :parameters item))))

(defn nodes [z]
  (map (comp plain-node zip/node)
       (take-while #(not (zip/end? %)) (iterate zip/next z))))

(defn by-key [k vals]
  (fn [n]
    (if n (some (set vals) [(n k)]))))

(defn by-name
  [testnames]
   (by-key :name testnames))

(defn by-tag
  [testtags]
  (by-key :tags testtags))

(defn filter-tests [pred]
  (fn [z]
    (filter pred (-> z zip/root test-zip nodes))))

(defn combine "combines two thunks into one, using juxt"
  [f g]
  (let [[sf sg] (for [i [f g]] (-> (meta i) ::source))]
    (with-meta (juxt f g) (merge (meta f) (meta g)
                                 (if (and sf sg)
                                   {::source (concat sf (drop 2 sg))}
                                   {})))))


(defn before-test "Run f before the steps of test node n" [f n]
  (let [s (:steps n)]
    (assoc n :steps (combine f s))))

(defn after-test "Run f after the steps of test node n" [f n]
  (let [s (:steps n)]
    (assoc n :steps (combine s f))))

(defn run-before "Run f before every test that matches pred"
  [pred f tree]
  (walk-all-matching pred (partial before-test f) tree))

(defn add-pre [pred t]
  (let [b (:blockers t)]
    (assoc t :blockers (every-p? b pred))))

(defn before-all [f n]
  (run-before (complement :configuration) f n))

(defn wrap-graceful-exit [consume-fn]
  (fn [] (try (consume-fn)
              (catch Exception e (do (reset! q nil) (throw e))))))

;;
;;post-execution reporting functions
;;
(defn report [test]
  (let [v (@reports test)]
    (assert v)
    (assert (deref v))
    (deref v)))

(defn blocked-by [test]
  (-> test report :blocked-by))

(defn result [test]
  (-> test report :result))

(defn thread [test]
  (-> test report :thread))

(defn passed? [test]
  (= (result test) :pass))

(defn configuration? [test]
  (boolean (:configuration test)))

(defn skipped-tests []
  (filter (fn [t] (and (not (configuration? t))
                       (= (result t) :skip))) (keys @reports)))

(defn passed-tests []
  (filter (fn [t] (and (not (configuration? t))
                            (= (result t) :pass))) (keys @reports)))

(defn failed-tests []
  (filter (fn [n] 
            (isa? (class (result n)) Throwable)) (keys @reports)))

(defn execution-time [test]
  (let [report (-> (@reports test) deref)
        start (report :start-time)
        end (report :end-time)]
    (if (and start end)
      (/ (- end start) 1000.0)
      0)))

(defn total-time []
  (reduce + (map execution-time (keys @reports))))

(defn junit-report []
  (let [fails (failed-tests)
        skips (skipped-tests)
        passes (passed-tests)
        [numfail numskip numpass] (map count [fails skips passes])
        total (+ numfail numskip numpass)
        info (fn [t] {:name (or (:parameters t) (:name t))
                     :time (execution-time t)
                     :classname (:name t)})]
    (with-out-str
      (xml/prxml [:decl! {:version "1.0"} ]
                 [:testsuite {:tests (str total)
                              :failures (str numfail)
                              :errors "0"
                              :skipped (str numskip)
                              :time (str (total-time))}
                  (concat (for [fail fails]
                            [:testcase (info fail)
                             [:failure {:type (->  fail result class .getCanonicalName )
                                        :time (execution-time fail)
                                        :message (format "On thread %s: %s"
                                                         (thread fail)
                                                         (-> fail result .getMessage))}
                              [:cdata! (-> fail result st/print-cause-trace with-out-str)]]])
                          (for [skip skips]
                            (let [reason (blocked-by skip)]
                              [:testcase (info skip)
                               [:skipped (if reason
                                           {:message (format "On thread %s: %s"
                                                             (thread skip)
                                                             (str reason))}
                                           {})]]))
                          (for [pass passes]
                            [:testcase (info pass)]))]))))
;;test execution functions

(defn execute [name proc]
  (proc))

(defn execute-procedure "Executes test, calls listeners, returns either :pass
                    if the test exits normally,
                    :skip if a dependency failed, or an exception the test threw." 
  [test]    
  (let [start-time  (System/currentTimeMillis)]
    (merge (try {:returned (execute (:name test)
                                    (:steps test)) ;test fn is called here
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
  (println (str "running test: " (:name (zip/node z))) )
  (let [this-test (-> z zip/node plain-node)]
    (try (let [all-blockers (concat blockers (parent-blocker z))
               blocked? (-> all-blockers count (> 0))
               report (merge {:thread (.getName (Thread/currentThread))}
                             (if (or (:always-run this-test)
                                     (not blocked?))
                               (execute-procedure this-test)
                               (let [timestamp (System/currentTimeMillis)]
                                 (merge {:result :skip
                                         :start-time timestamp
                                         :end-time timestamp}
                                        (if blocked?
                                          {:blocked-by all-blockers} {})))))]
           (deliver (@reports this-test)
                    report)
           (println "report delivered: "  (:name this-test) ": "
                    (dissoc report :start-time :end-time)))
         (catch Exception e
           (deliver (@reports this-test) {:result e})
           (println "report delivered with error: "  (:name this-test) ": " e))))
  (doseq [child-test (child-locs z)]
    (queue child-test)))

(defn consume []
  (while (and @q
              (not (and @done
                        (.isEmpty @q))))
    (if-let [next-item (.poll @q (long 500) TimeUnit/MILLISECONDS)]
      (next-item)))
  (if-not @q (println "queue reset, thread exiting.")
          (println "thread done.")))

(defn queue [z]
  (future
    (let [blockers (try
                       ((or (-> z zip/node :blockers)
                            (constantly []))  ;;default blocker fn returns empty list
                         z)
                       (catch Exception e [e]))]  
      (println (str "queueing: " (-> z zip/node :name)))
      (.offer @q (fn [] (run-test z blockers))))))

(defn run-allp [tree]
  (let [thread-runner (or (-> tree meta :thread-runner) identity)
        setup (or (-> tree meta :setup) (constantly nil))
        teardown (or (-> tree meta :teardown) (constantly nil))
        numthreads (or (-> tree meta :threads) 1)
        z (test-zip tree)] 
    
    (reset! q (LinkedBlockingQueue.))
    (reset! done false)
    (reset! reports (zipmap (nodes z)
                            (repeatedly promise)))
    (let [end-wait (future ;;;when all reports are done, raise 'done' flag
                           ;;;and do teardown
                         (doall (map deref (vals @reports)))
                         (reset! done true) 
                         (teardown))]
      (setup)
      (doseq [agentnum (range numthreads)]
        (.start (Thread. (-> consume
                             thread-runner)
                         (str "test.tree-thread" agentnum))))
      (queue z)
      end-wait)))

(defn run-suite [tree]
  @(run-allp tree)
  (spit "junitreport.xml" (junit-report))
  (spit "report.clj" (with-out-str
                       (binding [pprint/*print-right-margin* 120
                                 pprint/*print-suppress-namespaces* true
                                 pprint/*print-miser-width* 80]
                         (pprint/pprint (sort-by (fn [item] (-> item :report :start-time))
                                                 (map #(assoc %1 :report %2)
                                                      (keys @reports)
                                                      (map deref (vals @reports))))))))
  @reports)


 
(def myvar "hi")

(def sample (with-meta {:name "login"
                        :steps (fn [] (Thread/sleep 2000) (println "logged in"))
                        :more [{:name "create a widget"
                                :steps (fn [] (Thread/sleep 3000) (println "widget created") (throw (Exception. "woops"))) }
                               {:name "create a sprocket"
                                :steps (fn [] (Thread/sleep 5000) (println (str "sprocket created " myvar )))
                                :more [{:name "send a sprocket via email"
                                        :steps (fn [] (Thread/sleep 4000) (println "sent sprocket"))}]}
                               {:name "create a frob"
                                :steps (fn [] (Thread/sleep 4000) (println "frob created"))
                                :more [{:name "rename a frob"
                                        :steps (fn [] (Thread/sleep 4000) (println "frob renamed"))}
                                       {:name "delete a frob"
                                        :steps (fn [] (Thread/sleep 4000)
                                                 (throw (Exception. "woops, frob could not be deleted."))
                                                 (println "frob deleted"))
                                        :more [{:name "undelete a frob"
                                                :steps (fn [] (Thread/sleep 2000 (println "frob undeleted.")))}]}
                                       {:name "make sure 2 frobs can't have the same name"
                                        :steps (fn [] (Thread/sleep 4000) (println "2nd frob rejected"))}

                                       {:name "do that4"
                                        :steps (fn [] (Thread/sleep 4000) (println (str "there2.4 " myvar)))}
                                       {:name "do that5"
                                        :blockers (filter-tests (every-p? (by-name ["delete a frob"])
                                                                          (complement passed?)))
                                        :steps (fn [] (Thread/sleep 4000) (println "there2.5"))}
                                       {:name "do that6"
                                        :blockers (filter-tests (every-p? (by-name  ["final"]) (complement passed?)))
                                        :steps (fn [] (Thread/sleep 4000) (println (str "there2.6 " myvar)))}
                                       {:name "do that7"
                                        :blockers (filter-tests (every-p? (by-name ["do that2"]) (complement passed?)))
                                        :steps (fn [] (Thread/sleep 4000) (println "there2.7"))}]}
                               {:name "borg4"
                                :steps (fn [] (Thread/sleep 5000) (println "there4"))
                                :more [{:name "final"
                                        :steps (fn [] (Thread/sleep 4000) (println "there4.1"))}]}]}
              {:threads 4}
               ;:thread-runner (fn [c] (throw (Exception. "waah")))
               ))

