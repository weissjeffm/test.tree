(ns test-clj.core
  (:require [clojure.set :as set]))

(def sort-tests nil)
(def listeners (atom []))
(def config-map 
     {:before-suite    -3
      :before-group    -2
      :before-test     -1
      nil              0
      :after-test      1
      :after-group     2
      :after-suite     3})


					;--- listener calls
;; (defn notify [result test listener]
;;   (let [resulttype (class result)
;; 	config (meta/configuration test)
;; 	config-events {:pass :onConfigurationFinish :skip onConfigurationSkip
;; 		       event ]))
  
(comment (defmulti test-end-notify  (fn [result test listener] [(class result) (meta/configuration test) nil]))
 (defmethod test-end-notify [clojure.lang.Keyword nil nil] [result test listener]
   (let [test-event-map {:pass :onTestFinish :skip :onTestSkip}
         configuration (meta/configuration test)]
     
     (cond (nil? configuration)	     
           ((listener (test-event-map result)) test result)
           true ())))
 (defmethod test-end-notify [java.lang.Throwable nil nil] [result test listener]
   ((listener :onTestFail) test result))


 (defn test-start-notify [test listener] 
   (let [configuration (meta/configuration test)]
     (cond (nil? configuration) ()))))
					;--- end listener calls
(defn dependencies "Get the test set for all the dependencies of a test"
  [all-tests test]
  (let [test-deps (or (:depends-on test) {})]
    (mapcat #(filter % all-tests) [ (fn [t] (some #{(:name t)} (:tests test-deps)))
                                    (fn [t] (some (or (:groups t) #{}) (:groups test-deps)))])))

(defn passed? [test]
  (= :pass (:result test)))

(defn dependencies-met?
  "Returns true if all the tests listed as dependencies for this test have passed."
  [test-list test]
  (every? passed? (dependencies test-list test)))

(defn execute-test "Executes test, calls listeners, returns either :pass
                    if the test exits normally,
                    :skip if a dependency failed, or an exception the test threw." 
  [test]    
  
  (let [start-time  (System/currentTimeMillis)]
    (assoc test
      :result (try ((:procedure test))             ;test fn is called here 
                   :pass
                   (catch Exception e e)) 
      :start-time start-time
      :end-time (System/currentTimeMillis)))
  ;;	cell-listeners (fn [ltype] (doseq [listener listeners] ;todo - make this happen before and after
  ;;	((listener ltype) result)))
      
  )

(defn run-tests [tests] "Runs all tests in the coll tests.  Returns a
                         vector of tests with results included."
  (loop [remaining-tests tests 
         finished-tests []] 
    (if (empty? remaining-tests) finished-tests	 
        (let [test (first remaining-tests)] 
          (recur 
           (rest remaining-tests) 
           (conj finished-tests
                 (if (dependencies-met? finished-tests test)
                   (execute-test test)
                   (assoc test :result :skip))))))))

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

(defn in-dependency-chain?
  "Returns true if target is in the dependency chain of test1"
  ([test1 target] (in-dependency-chain? test1 target #{}))
  ([test1 target deps-visited]
     (let [deps1 (meta/dependencies test1)]
       ;;(println (format "Is %s a dep of %s?" (:name (meta target)) (:name (meta test1))))
       (cond 
	(empty? deps1)   false 
	(contains? deps1 target)   true 
	(not (empty? (set/intersection deps1 deps-visited))) 
	  (throw (IllegalStateException. (format "%s has a cyclic dependency." (:name (meta test1)))))
	:else (some #(in-dependency-chain? % target (conj deps-visited test1))
		    deps1)))))

(defn compare-using "Will run through the comparators, in order, until one finds a difference.
                    In that case, that comparator's return value is returned, otherwise
                    returns 0."
  [comps arg1 arg2] 
  (or (first (drop-while zero? (map #(% arg1 arg2) comps))) 
      0)) ;turn nil into 0

(defn compare-deps [test1 test2]
  (if (in-dependency-chain? test2 test1) ;run test1 before test2
    1
    (if (in-dependency-chain? test1 test2) ;run test2 before test1
      -1
      0)))

(defn compare-configuration "Compares two configuration functions to see which one should run first.
                             For instance, beforeSuite runs before beforeNS.  Returns an integer
                             representing the relative order of the two functions."
  [test1 test2]
  (reduce - (map (comp meta/config-map meta/configuration) 
		 (list test1 test2))))

(defn test-comparator [arg1 arg2]
  (if (not (and (map? arg1) ;first check args are right type
                (map? arg2)))
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
