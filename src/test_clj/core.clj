(ns test-clj.core
  (:require [test-clj.meta :as meta]
	    [test-clj.results :as results]))

(def sort-tests nil)
(def listeners (atom []))

					;--- listener calls
(defmulti test-end-notify  (fn [result test listener] [(class result) nil nil]))
(defmethod test-end-notify [clojure.lang.Keyword nil nil] [result test listener]
  (let [listener-map {:pass :onTestPass :skip :onTestSkip}]
    ((listener (listener-map result)) test result)))
(defmethod test-end-notify [java.lang.Throwable nil nil] [result test listener]
   ((listener :onTestFail) test result))

(defn test-start-notify [test listener] 
  (let [configuration (meta/configuration test)]
    (cond () ())))
					;--- end listener calls
(defn dependencies-met? [test]
  (let [deps (meta/dependencies test)]
       ))

(defn execute-test "Executes test, calls listeners, returns either :pass
                    if the test exits normally,
                    :skip if a dependency failed, or an exception the test threw." 
  [test results]    
  (let [dep (meta/dependencies test)]
    ;;	cell-listeners (fn [ltype] (doseq [listener listeners] ;todo - make this happen before and after
    ;;	((listener ltype) result)))
      
    (let [parameters   (meta/parameters test)
	  test-result  {:startTime (System/currentTimeMillis)
			:parameters parameters}]
      (if (not (dependencies-met? test))
	(assoc test-result :result :skip) 
	(assoc  (try 
		  (apply test parameters) 
		  (assoc test-result :result :pass)
		  (catch Exception e (assoc test-result :result e)))
	  :endTime (System/currentTimeMillis))))))

(defn gather-tests [testfilter nslist]
  (->> nslist (map ns-publics) (apply concat) vals (filter testfilter)))

(defn run-tests-matching "Runs all tests, in the coll of namespaces in nslist,
                         using the testfilter-fn to filter out any tests that 
                         shouldn't be run.  Returns a map of test fn's to their result."
  ([] (run-tests-matching meta/test? [*ns*])) ;by default run the tests in current ns
  ([nslist] (run-tests-matching meta/test? nslist))
  ([testfilter nslist]
     (let [tests (->> (gather-tests testfilter nslist) sort-tests)]
       (loop [remaining-tests tests 
	      results []] 
	 (if (empty? remaining-tests) results	 
	     (let [test (first remaining-tests)] 
	       (recur 
		(rest remaining-tests) 
		(conj results {test (execute-test test results)}))))))))

(defn insert-before-after-tests [tests]
  (let [config meta/configuration
	filter-by (fn [config-type] (filter #(= (config %) config-type) tests))
	slice-by (fn [suite ns] (take-while #(or (= (config %) suite )
						 (= (config %) ns))  
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

(defn compare-using "Will run through the comparators, in order, until one finds a difference.
                    In that case, that comparator's return value is returned, otherwise
                    returns 0."
  [comps arg1 arg2] 
  (or (first (drop-while zero? (map #(% arg1 arg2) comps))) 
      0)) ;turn nil into 0

(defn compare-deps [test1 test2]
  (let [deps1 (meta/dependencies test1)
        deps2 (meta/dependencies test2)]
    (if (contains? deps1 test2) ;run test1 before test2
      1
      (if (contains? deps2 test1) ;run test2 before test1
        -1
        0))))

(defn compare-configuration "Compares two configuration functions to see which one should run first.
                             For instance, beforeSuite runs before beforeNS.  Returns an integer
                             representing the relative order of the two functions."
  [test1 test2]
  (reduce - (map (comp meta/config-map meta/configuration) 
		 (list test1 test2))))

(defn test-comparator [arg1 arg2]
  (if (not (and (var? arg1) ;first check args are right type
                (var? arg2)))
    (throw (IllegalArgumentException. 
	    (format "Both arguments should be a var. Got: %s, %s" arg1 arg2))))
  (compare-using [compare-configuration compare-deps] arg1 arg2))

(defn sort-tests [tests] 
  (insert-before-after-tests (sort test-comparator tests)))

(defn add-listener [listener]
  (if (map? listener) 
    (conj listeners listener)
    (throw (IllegalArgumentException. 
	    (format "Argument should be a map (of keywords to functions). Got: %s" listener)))))

(def log-listener
     {:onTestStart (fn [result] (println (format "Starting test %s" )))
      })