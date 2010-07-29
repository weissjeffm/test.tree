(ns test-clj.core)
(def test? )
(def sorted-tests nil)
(def dependency nil)
(def configuration nil)
(def test2 nil)

(def listeners (atom []))

					;--- listener calls
(defmulti test-end-notify  (fn [result test listener] [(class result) nil nil]))
(defmethod test-end-notify [clojure.lang.Keyword nil nil] [result test listener]
  (let [listener-map {:pass :onTestPass :skip :onTestSkip}]
    ((listener (listener-map result)) test result)))
(defmethod test-end-notify [java.lang.Throwable nil nil] [result test listener]
   ((listener :onTestFail) test result))

(defn test-start-notify [test listener] 
  (let [configuration (configuration test)]
    (cond () ())))
					;--- end listener calls

(defn results-map [results] 
  (into {} results))

(defn execute-test "Executes test, calls listeners, returns either :pass
                    if the test exits normally,
                    :skip if a dependency failed, or an exception the test threw." 
  [test results]    
  (let [dep (dependency test)]
;;	cell-listeners (fn [ltype] (doseq [listener listeners] ;todo - make this happen before and after
;;	((listener ltype) result)))

    (if (and dep (not= ((results-map results) dep)
		       :pass))
      :skip
      (try (test) 
	   :pass
	   (catch Exception e e)))))

(defn run-tests-matching "Runs all tests, in the coll of namespaces in nslist,
                         using the testfilter-fn to filter out any tests that 
                         shouldn't be run.  Returns a map of test fn's to their result."
  ([] (run-tests-matching test? [*ns*])) ;by default run the tests in current ns
  ([nslist] (run-tests-matching test? nslist))
  ([testfilter nslist]
     (let [tests (sorted-tests (filter testfilter 
				       (vals (apply concat
						    (map ns-publics nslist)))))]
       (loop [remaining-tests tests 
	      results []] 
	 (if (empty? remaining-tests) results	 
	     (let [test (first remaining-tests)] 
	       (recur 
		(rest remaining-tests) 
		(conj results {test (execute-test test results)}))))))))

(defn dependency [test]
  (-> (meta test) :test :dependsOnTest))

(defn configuration [test]
  (-> (meta test) :test :configuration))

(defn in-group? [group myfn]
  (contains? 
   (->(meta myfn) :test :groups) 
   group))

(defn in-groups? [groups myfn]
  (some #(in-group? % myfn) groups))

(defn insert-before-after-tests [tests]
  (let [filter-by (fn [config-type] (filter #(= (configuration %) config-type) tests))
	slice-by (fn [suite ns] (take-while #(or (= (configuration %) suite )
						 (= (configuration %) ns))  
					    tests))
	before-tests (filter-by :beforeTest)   
	after-tests (filter-by :afterTest)
	plain-tests (filter-by nil)]
					;concat 3 sections together: 
					;all the before configs,
					;all the tests w beforeTest/afterTests
					;all the after configs
    (concat (slice-by :beforeSuite :beforeNS)
	    (apply concat (map (fn [test] (concat before-tests [test] after-tests))
			       plain-tests))
	    (slice-by :afterSuite :afterNS))))

(defn test? [myfn]
  (contains? (meta myfn) :test))

(defn compare-using "Will run through the comparators, in order, until one finds a difference.
                    In that case, that comparator's return value is returned, otherwise
                    returns 0."
  [comps arg1 arg2] 
  (or (first (drop-while zero? (map #(% arg1 arg2) comps))) 
      0)) ;turn nil into 0

(defn compare-deps [test1 test2]
  (let [dep1 (dependency test1)
        dep2 (dependency test2)
	isdep (fn [dep test] (and dep 
			     (= dep test)))]
    (if (isdep dep1 test2) ;run test1 before test2
      1
      (if (isdep dep2 test1) ;run test2 before test1
        -1
        0))))

(def config-map 
     {:beforeSuite -3
      :beforeNS    -2
      :beforeTest  -1
      nil          0
      :afterTest   1
      :afterNS     2
      :afterSuite  3})

(defn compare-configuration [test1 test2]
  (reduce - (map (comp config-map configuration) 
		 (list test1 test2))))

(defn compare-tests-order [arg1 arg2]
					;first check args are right type
  (if (not (and (var? arg1) 
                (var? arg2)))
    (throw (IllegalArgumentException. 
	    (format "Both arguments should be a var. Got: %s, %s" arg1 arg2))))
  (compare-using [compare-configuration compare-deps] arg1 arg2))

(defn sorted-tests [tests] 
  (insert-before-after-tests
   (sort compare-tests-order tests)))

(defn add-listener [listener]
  (if (map? listener) 
    (conj listeners listener)
    (throw (IllegalArgumentException. 
	    (format "Argument should be a map (of keywords to functions). Got: %s" listener)))))

(def log-listener
     {:onTestStart (fn [result] (println (format "Starting test %s" )))
      })