(ns test-clj.core
  (:require [clojure.zip :as zip]
            [clojure.pprint :as pprint]
            [clojure.stacktrace :as st]
            [clojure.contrib.prxml :as xml])
  (:use [clojure.contrib.core :only [-?>]])
  (:refer-clojure :exclude [fn])
  (import (java.util.concurrent Executors ExecutorService Callable ThreadFactory)))

(def *pool* (atom nil))

(defmacro ^{:doc (str (:doc (meta #'clojure.core/fn))
                              "\n\n  Oh, but it also allows serialization!!!111eleven")}
          fn [& sigs]
          `(with-meta (clojure.core/fn ~@sigs)
             {:type ::serializable-fn
              ::source (quote ~&form)}))



(defmethod print-method ::serializable-fn [o ^Writer w]
  (print-method (::source (meta o)) w))

(defn test-zip [tree] (zip/zipper (constantly true)
                                  :more 
                                  (fn [node children]
                                    (with-meta (conj node {:more children}) (meta node)))
                                    tree))

(defn walk-all "Does a depth-first walk of the tree, passes each node thru f, and returns the tree"
  [f tree]
  (first (drop-while (complement zip/end?)
                     (iterate (fn [l]
                                (let [new-l (zip/edit l f)] 
                                  (zip/next new-l))) tree))))

(defn walk-all-matching [node-pred f tree]
  (walk-all (fn [n] ((if (node-pred n) f
                        identity) n))
            tree))

(defn child-locs [tree]
  (take-while (fn [l] (some #{(and l (zip/node l))} 
                           (zip/children tree)))
              (iterate zip/right (zip/down tree))))

(defn result [test]
  (-> (:report test) deref :result))

(defn failed-pre [test]
  (-> (:report test) deref :failed-pre))

(defn passed? [test]
  (= (result test) :pass))

(defn configuration? [test]
  (boolean (:configuration test)))

(defn execute [name proc]
  (proc))

(defn execute-procedure "Executes test, calls listeners, returns either :pass
                    if the test exits normally,
                    :skip if a dependency failed, or an exception the test threw." 
  [test]    
  (let [start-time  (System/currentTimeMillis)]
    {:result (try (execute (:name test)
                           (:steps test))             ;test fn is called here
                  :pass
                  (catch Throwable t t)) 
     :start-time start-time
     :end-time (System/currentTimeMillis)}))

(declare queue)

(defn add-promises [data]
  (zip/root (first (drop-while (fn [l]  (not (zip/end? l)))
                      (iterate (comp zip/next (fn [l] (zip/edit l assoc :report (promise))))
                               (test-zip data))))))

(defn run-test [tree failed-pre]
  (println (str "running test: " (:name (zip/node tree))) )
  (let [this-test (zip/node tree)
        direct-dep (zip/up tree)
        dd-passed? (if direct-dep
                     (passed? (zip/node direct-dep))
                     true)
        deps-passed? (and dd-passed? (not failed-pre))]
    (deliver (:report this-test)
             (if (or (:always-run this-test)
                     deps-passed?)
               (execute-procedure this-test)
               {:result :skip
                :failed-pre failed-pre}))
    (doseq [child-test (child-locs tree)]
      (queue child-test))
    tree))

(defn queue [tree]
  (future
    (let [failed-pre ((or (:pre (zip/node tree)) (constantly nil)) (zip/root tree))]  
      (println (str "queueing: " (:name (zip/node tree))))
      (.submit ^ExecutorService @*pool*
              ^Callable (identity (fn [] (run-test tree failed-pre)))))))

(defn run-allp [data]
  (let [thread-init (-> data meta :thread-init)
        numthreads (or (-> data meta :threads) 1)] 
    (reset! *pool*
            (if thread-init
             (Executors/newFixedThreadPool 3 (reify ThreadFactory
                                               (newThread [this r]
                                                          (Thread. (reify Runnable
                                                                     (run [this]
                                                                          (thread-init)
                                                                          (.run r)))))))
             (Executors/newFixedThreadPool 3)))
    (-> (add-promises data) test-zip queue deref .get zip/root)))

(defn data-driven "Generate a set of n data-driven tests from a template
                   test, a function f that takes p arguments, and a n by p list
                   of lists containing the data for the tests."
  [test f data]
  (for [item data] (merge (or (meta data) {})
                          (assoc test
                            :steps (with-meta (apply partial f item) (meta f))
                            :parameters item))))

(defn plain-node [n]
  (dissoc n :more))

(defn nodes [z]
  (map (comp plain-node zip/node) (take-while #(not (zip/end? %)) (iterate zip/next z))))

;;helper functions

(defn by-field [k vals]
  (fn [n]
    (if n (some (set vals) [(n k)]))))

(defn by-name
  [tests]
   (by-field :name tests))

(defn unsatisfied [pred]
  (fn [rootnode] (let [unsat (filter #(and (pred %1)
                                          ((complement passed?) %1))
                                    (nodes (test-zip rootnode)))]
                  (if (= 0 (count unsat)) nil
                      unsat))))

(defn filter-tests [z p]
  (filter p (nodes z)))

(defn skipped-tests [z]
  (filter-tests z (fn [n]
                  (and (not (configuration? n))
                       (= (result n) :skip)))))

(defn passed-tests [z]
  (filter-tests z (fn [n] (and (not (configuration? n))
                            (= (result n) :pass)))))

(defn failed-tests [z]
  (filter-tests z (fn [n] (and (not (configuration? n))
                             (isa? (class (result n)) Exception)))))

(defn execution-time [n]
  (let [report (-> n :report deref)
        start (report :start-time)
        end (report :end-time)]
    (if (and start end)
      (/ (- end start) 1000.0)
      0)))

(defn total-time [z]
  (reduce + (map execution-time (nodes z))))

(defn junit-report [z]
  (let [fails (failed-tests z)
        skips (skipped-tests z)
        passes (passed-tests z)
        [numfail numskip numpass] (map count [fails skips passes])
        total (+ numfail numskip numpass)
        info (fn [n] {:name (or (:parameters n) (:name n))
                     :time (execution-time n)
                     :classname (:name n)})]
    (with-out-str
      (xml/prxml [:decl! {:version "1.0"} ]
                 [:testsuite {:tests (str total)
                              :failures (str numfail)
                              :errors "0"
                              :skipped (str numskip)
                              :time (str (total-time z))}
                  (concat (for [fail fails]
                            [:testcase (info fail)
                             [:failure {:type (-> (result fail) class .getCanonicalName )
                                        :time (execution-time fail)
                                        :message (-> (result fail) .getMessage)}
                              [:cdata! (-> (result fail) st/print-cause-trace with-out-str)]]])
                          (for [skip skips]
                            (let [reason (failed-pre skip)]
                              [:testcase (info skip)
                               [:skipped (if reason
                                           {:message (str reason)}
                                           {})]]))
                          (for [pass passes]
                            [:testcase (info pass)]))]))))

(defn run-suite [tests]
  (let [result (run-all (test-zip tests))
        fresh-result (-> result zip/root test-zip)]
    (pprint/pprint result)
    (spit "junitreport.xml" (junit-report fresh-result))
    (zip/root fresh-result)))

(defn combine "combines two thunks into one, using juxt"
  [f g]
  (let [[sf sg] (for [i [f g]] (-> (meta i) ::source))]
    (with-meta (juxt f g) (merge (meta f) (meta g)
                                 (if (and sf sg)
                                   {::source (concat sf (drop 2 sg))}
                                   {})))))

(defn matching-all-tags [ & tags]
  (fn [n] (some (set tags) (:tags n))))

(defn before-test "Run f before the steps of test node n" [f n]
  (let [s (:steps n)]
    (assoc n :steps (combine f s))))

(defn after-test "Run f after the steps of test node n" [f n]
  (let [s (:steps n)]
    (assoc n :steps (combine s f))))

(defn run-before "Run f before every test that matches pred"
  [pred f n]
  (->> (test-zip n)
       (walk-all-matching pred (partial before-test f))
       zip/node))

(defn before-all [f n]
  (run-before (complement :configuration) f n))
 

(def sample (with-meta {:name "blah"
               :steps (fn [] (Thread/sleep 2000) (println "root"))
               :more [{:name "borg"
                       :steps (fn [] (Thread/sleep 3000) (println "there") (throw (Exception. "woops"))) }
                      {:name "borg3"
                       :steps (fn [] (Thread/sleep 5000) (println "there3"))
                       :more [{:name "do the other"
                               :steps (fn [] (Thread/sleep 4000) (println "there3.1"))}]}
                      {:name "borg2"
                       :steps (fn [] (Thread/sleep 4000) (println "there2"))
                       :more [{:name "do that"
                               :steps (fn [] (Thread/sleep 4000) (println "there2.1"))}
                              {:name "do that2"
                               :steps (fn [] (Thread/sleep 4000) (println "there2.2") (throw (Exception. "woops")))}
                              {:name "do that3"
                               :steps (fn [] (Thread/sleep 4000) (println "there2.3"))}

                              {:name "do that4"
                               :steps (fn [] (Thread/sleep 4000) (println "there2.4"))}
                              {:name "do that5"
                               :pre (unsatisfied (by-name ["do the other"]))
                               :steps (fn [] (Thread/sleep 4000) (println "there2.5"))}
                              {:name "do that6"
                               :pre (unsatisfied (by-name ["final"]))
                               :steps (fn [] (Thread/sleep 4000) (println "there2.6"))}
                              {:name "do that7"
                               :pre (unsatisfied (by-name ["do that2"]))
                               :steps (fn [] (Thread/sleep 4000) (println "there2.7"))}]}
                      {:name "borg4"
                       :steps (fn [] (Thread/sleep 5000) (println "there4"))
                       :more [{:name "final"
                               :steps (fn [] (Thread/sleep 4000) (println "there4.1"))}]}]}
              {:threads 4
               :thread-init (fn [] (println "starting thread!"))}))

