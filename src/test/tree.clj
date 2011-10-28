(ns test.tree
  (:require [clojure.zip :as zip]
            [clojure.pprint :as pprint] 
            [clojure.contrib.prxml :as xml]
            [clojure.data :as data])
  (:use [clojure.contrib.core :only [-?>]]
        [pretzel.combine :only [every-p?]]
        [clj-stacktrace.repl :only [pst-str]])
  (:refer-clojure :exclude [fn])
  (import (java.util.concurrent Executors ExecutorService Callable ThreadFactory
                                TimeUnit LinkedBlockingQueue ThreadPoolExecutor )))

(def q (atom nil))
(def done (atom nil))
(def suite (ref nil))

(defn print-meta [val]
  {:type ::serializable-fn
   ::source val})

(defmacro ^{:doc (str (:doc (meta #'clojure.core/fn))
                              "\n\n  Oh, but it also allows serialization!!!111eleven")}
          fn [& sigs]
          `(with-meta (clojure.core/fn ~@sigs)
             (print-meta (quote ~&form))))


(defmethod print-method ::serializable-fn [o ^java.io.Writer w]
  (print-method (::source (meta o)) w))

;;
;;pre-execution test manipulation functions
;;
(declare passed?)

(defn test-zip "Create a clojure.zip structure so the tree can be easily walked."
  [tree]
  (zip/zipper (constantly true)
              :more 
              (fn [node children]
                (with-meta (conj node {:more children}) (meta node)))
              tree))

(defn path "Return a path to current node that can be used by get-in or update-in"
  [z]
  (->> (take-while identity (iterate zip/up z))
     (map #(count (zip/lefts %)))
     butlast
     reverse
     (interleave (repeat :more))))

(defn diff-to-path "'Undoes' a diff - returns info necessary to update-in, (the path and what was added)
                    Note: only works on changes that can be done with update-in + assoc"
  [[a b both]]
  (comment {:more [nil nil {:more [nil {:hi :there}]}]} :more 2 :more 1)
  (let [idx (fn [m] (count (take-while nil? (:more m))))
        nested (fn [m] (-> m :more last))
        [path added] (loop [m b acc []]
                       (comment (println m))
                       (if (or (nil? m) (nil? (:more m)))
                         [acc m]
                         (recur (nested m) (conj acc (idx m)))))]
    [(vec (interleave (repeat :more) path)) added]))

(defn walk-all "Does a depth-first walk of the tree, passes each node
                thru f, and returns the tree"
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

(defn data-driven "Generate a set of n data-driven tests from a
                   template test, a function f that takes p arguments,
                   and a n by p coll of colls containing the data for
                   the tests. The metadata on either the overall set,
                   or rows of data, will be extracted and merged with
                   the tests"
  [test f data]
  (for [item data] (merge (or (meta data) {})
                          (or (meta item) {})
                          (assoc test
                            :steps (with-meta (apply partial f item) (meta f))
                            :parameters item))))

(defn dep-chain "Take a list of tests and nest them as a long tree branch"
  [tests]
  (vector (reduce #(assoc %2 :more [%1]) (reverse tests))))

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


;;
;; listener functions
;;



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
  (-> test report :status))

(defn thread [test]
  (-> test report :thread))

(defn passed? [test]
  (= (result test) :passed))

(defn configuration? [test]
  (boolean (:configuration test)))

(defn skipped-tests []
  (filter (fn [t] (and (not (configuration? t))
                       (= (result t) :skipped))) (keys @reports)))

(defn passed-tests []
  (filter (fn [t] (and (not (configuration? t))
                            (= (result t) :passed))) (keys @reports)))

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

(defn junit-report "Produce an xml report consistent with the
                    junit report schema.  Tries to be especially
                    compatible with Jenkins and ReportNG." []
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
                              [:cdata! (-> fail result pst-str)]]])
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

(defn execute "Executes test, calls listeners, returns either :passed
                    if the test exits normally,
                    :skipped if a dependency failed, or an exception the test threw." 
  [test]    
  (let [start-time  (System/currentTimeMillis)]
    (merge (try {:returned ((:steps test)) ;test fn is called here
                 :status :passed}
                (catch Throwable t {:status t}))
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
  (let [this-test (-> z zip/node plain-node)
        path-to-this-test (path z)]
    (try (let [all-blockers (concat blockers (parent-blocker z))
               blocked? (-> all-blockers count (> 0))
               report (merge {:thread (.getName (Thread/currentThread))}
                             (if (or (:always-run this-test)
                                     (not blocked?))
                               (do
                                 (dosync
                                  (alter suite update-in path-to-this-test {:status :running}))
                                 (execute (plain-node this-test)))
                               (let [timestamp (System/currentTimeMillis)]
                                 (merge {:status :skipped
                                         :start-time timestamp
                                         :end-time timestamp}
                                        (if blocked?
                                          {:blocked-by all-blockers} {})))))]
           
           (dosync (alter suite update-in path-to-this-test report))
           (println "report delivered: "  (:name this-test) ": "
                    (dissoc report :start-time :end-time)))
         (catch Exception e
           (dosync (alter suite update-in path-to-this-test {:status e}))
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
      (println (str "queueing: " (-> z zip/node :name)))
      (.offer @q (fn [] (run-test z blockers))))))

(defn run-allp [tree]
  (let [thread-runner (or (-> tree meta :thread-runner) identity)
        setup (or (-> tree meta :setup) (constantly nil))
        teardown (or (-> tree meta :teardown) (constantly nil))
        numthreads (or (-> tree meta :threads) 1)
        z (test-zip tree)] 
    
    (reset! q (LinkedBlockingQueue.))
    (dosync (ref-set suite tree))
    
    (setup)
    (doseq [agentnum (range numthreads)]
      (.start (Thread. (-> consume
                          thread-runner)
                       (str "test.tree-thread" agentnum))))
    (queue z)))

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
                                                      (map deref (vals @reports))))))))
  @reports)

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




