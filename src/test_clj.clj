(ns test-clj)
(def test? nil)
(def sorted-tests nil)
(def dependency nil)
(def test2 nil)
;sample tests
;------------------------------

(defn #^{:test {:groups #{:group1 :group2} 
                :dependsOnTest #'test2}}
  test1	[]
  (do (println "running test1")
      (println "test1 complete")))
	
(defn #^{:test {:groups #{:group2 :group3}}}
  test2	[]
  (do(println "running test2")
     (println "test2 complete")))
	
(defn #^{:test {:groups #{:group2 :group3}
		:dependsOnTest #'test2}}
  test3	[]
  (do (println "running test3")
      (throw (RuntimeException. "test failed!"))
      (println "test3 complete")))

(defn #^{:test {:groups #{:group1 :group3}
		:dependsOnTest #'test3}}
  test4	[]
  (do (println "running test4")
      ;;throw (RuntimeException. "test failed!"))
      (println "test4 complete")))

;end sample tests
;--------------------------------
	
(defn execute-test "returns results, with the test name, and 
either :pass, :skip, or an exception, conj'd onto the end" 
  [test prev-results]
  (let [dep (dependency test)
	result (if (and (not= dep nil) 
			(not= (dep prev-results) :pass))
		 :skip
		 (try (test) 
		      :pass
		      (catch Exception e e)))]
    (conj prev-results { test result })))
		
(defn run-tests-matching "Runs all tests using the testfilter-fn to filter
out any tests that shouldn't be run.  Returns a map of test fn's to their result."
  ([]
     (run-tests-matching test?))
  ([testfilter]
     (let [tests (sorted-tests (filter 
				testfilter 
				(vals (ns-publics *ns*))))]
       (loop [remaining-tests tests
	      results []]
	 (let [test (first remaining-tests)]
	   (if (empty? remaining-tests) results
	       (recur 
		(rest remaining-tests) 
		(conj results {test (execute-test test results)}))))))))

(defn dependency [test]
  (-> (meta test) :test :dependsOnTest))

(defn in-group? [group myfn]
  (contains? 
   (->(meta myfn) :test :groups) 
   group))

(defn test? [myfn]
  (contains? (meta myfn) :test))
	
(defn compare-tests-order [arg1 arg2]
  ;;first check args are right type
  (if (not (and (var? arg1) 
		(var? arg2))) 
    (throw (IllegalArgumentException. (format "Both arguments should be a var. Got: %s, %s" arg1 arg2))))
  (let [dep1 (dependency arg1)
	dep2 (dependency arg2)]
    (if (and (not (nil? dep1)) 
	     (= dep1 arg2)) ;run arg1 before arg2
      1
      (if (and (not (nil? dep2)) 
	       (= dep2 arg1)) ;run arg2 before arg1
	-1
	0)))) ;don't care what order

(defn sorted-tests [tests]
  (sort compare-tests-order tests))