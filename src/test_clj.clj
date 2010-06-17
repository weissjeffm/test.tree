(ns test-clj)
(def test? nil)
(def sorted-tests nil)
(def dependency nil)
(def test2 nil)
;sample tests
;------------------------------

(defn #^{:test {:groups #{:group1 :group2} 
                :dependsOnTests #'test2}}
  test1	[]
  (do (println "running test1")
      (println "test1 complete")))
	
(defn #^{:test {:groups #{:group2 :group3}}}
  test2	[]
  (do(println "running test2")
     (println "test2 complete")))
	
(defn #^{:test {:groups #{:group2 :group3}
		:dependsOnTests #'test2}}
  test3	[]
  (do (println "running test3")
      (throw (RuntimeException. "test failed!"))
      (println "test3 complete")))

(defn #^{:test {:groups #{:group1 :group3}
		:dependsOnTests #'test3}}
  test4	[]
  (do (println "running test4")
      ;;throw (RuntimeException. "test failed!"))
      (println "test4 complete")))
;;(defn #^{:beforeGroups []  })

;end sample tests
;--------------------------------
	
(defn execute-test "returns results, with the test name, and 
either :pass, :skip, or an exception, conj'd onto the end" 
  [test prev-results]
  (let [dep (dependency test)
	result (if (and (not= dep nil) 
			(not= (prev-results dep) :pass))
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
     (let [tests (sorted-tests 
		  (filter testfilter (vals (ns-publics *ns*))))]
       (loop [remaining-tests tests
	      results {}]
	 (let [test (first remaining-tests)]
	   (if (empty? remaining-tests) 
	     results
	     (recur 
	      (rest remaining-tests) 
	      (execute-test test results))))))))

(defn dependency [test]
  (let [dep-annotation (-> (meta test) :test :dependsOnTests)]
    (cond (or (nil? dep-annotation) 
	      (and (set? dep-annotation)
		   (every? var? dep-annotation)))     
	  dep-annotation
	  
	  (var? dep-annotation) #{dep-annotation}
	  
	  true (throw (IllegalArgumentException. ":dependsOnTests must contain either a set of vars or one var")))))


(defn in-group? [group myfn]
  (contains? 
   (->(meta myfn) :test :groups) 
   group))

(defn test? [myfn]
  (contains? (meta myfn) :test))
	
(defn compare-tests-order [test1 test2]
  ;;first check args are right type
  (if (not (and (var? test1) 
		(var? test2))) 
    (throw (IllegalArgumentException. (format "Both arguments should be a var. Got: %s, %s" test1 test2))))
  (let [deps1 (dependency test1)
	deps2 (dependency test2)]
    (if (and (not (nil? deps1)) 
	     (contains? deps1 test2)) ;run test2 before test1
      1
      (if (and (not (nil? deps2)) 
	       (contains? deps2 test1)) ;run test1 before test2
	-1
	0)))) ;don't care what order

(defn sorted-tests [tests]
  (sort compare-tests-order tests))

;;hmm...why didn't it run test1
;;test-clj> (run-tests-matching)
;;running test2
;;test2 complete
;;{#'test-clj/test4 :skip, #'test-clj/test3 :skip, #'test-clj/test1 :skip, #'test-clj/test2 :pass}