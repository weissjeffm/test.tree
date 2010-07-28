(ns test-clj)
(def test? )
(def sorted-tests nil)
(def dependency nil)
(def configuration nil)
(def test2 nil)

(def listeners (atom []))
					;sample tests
					;------------------------------

(defn ^{:test {:configuration :beforeSuite
                 :groups #{:group1 :group2}}} 
  config1 []
  (do (println "running configuration1")
      (println "configuration1 complete.")))

(defn ^{:test {:groups #{:group1 :group2} 
                :dependsOnTest #'test2}}
  test1 [] 
  (do (println "running test1") 
      (println "test1 complete")))

(defn ^{:test {:groups #{:group2 :group3}}} 
  test2 [] 
  (do(println "running test2")
     (println "test2 complete"))) 

(defn ^{:test {:groups #{:group2 :group3} 
                :dependsOnTest #'test2}} 
  test3 [] 
  (do (println "running test3") 
      (throw (RuntimeException. "test failed!")) (println "test3 complete")))

(defn ^{:test {:groups #{:group1 :group3} 
                :dependsOnTest #'test3}} 
  test4 [] 
  (do (println "running test4") 
      ;;(throw (RuntimeException. "test failed!")) 
      (println "test4 complete")))

(defn ^{:test {:groups #{:group1 :group2}
		:configuration :beforeTest}}
  config5 [] 
  (do (println "running configuration5") 
      (println "configuration5 complete")))

(defn ^{:test {:groups #{:group1 :group2}
		:configuration :afterTest}}
  config6 [] 
  (do (println "running configuration6") 
      (println "configuration6 complete")))

					;end sample tests
					;--------------------------------

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

(defn execute-test "Executes test, calls listeners, returns either :pass if the test exits normally,
 :skip if a dependency failed, or an exception the test threw." 
  [test results]
 
   
  (let [dep (dependency test)
;;	cell-listeners (fn [ltype] (doseq [listener listeners] ;todo - make this happen before and after
;;	((listener ltype) result)))
	result (if (and (not= dep nil)
			(not= ((results-map results) dep)
			      :pass))
		 :skip
		 (try (test) 
		      :pass
		      (catch Exception e e)))]))

(defn run-tests-matching "Runs all tests, in the coll of namespaces in nslist,
                         using the testfilter-fn to filter out any tests that 
                         shouldn't be run.  Returns a map of test fn's to their result."
  ([]
     (run-tests-matching test? [*ns*])) ;by default run the tests in this ns
  ([nslist]
     (run-tests-matching test? nslist))
  ([testfilter nslist]
     (let [tests (sorted-tests (filter testfilter 
				       (vals (apply concat
						    (map  ns-publics nslist)))))]
       (loop [remaining-tests tests 
	      results []] 
	 (let [test (first remaining-tests)] 
	   (if (empty? remaining-tests) results
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
	    (apply concat (map #(concat before-tests [%] after-tests)
				 plain-tests))
	    (slice-by :afterSuite :afterNS))))

(defn test? [myfn]
  (contains? (meta myfn) :test))

(defn compare-using "Will run through the comparators, in order, until one finds a difference.
                    In that case, that comparator's return value is returned, otherwise
                    returns 0."
  [comps arg1 arg2]
  (loop [remaining-comps comps]
    (if (empty? remaining-comps) 
      0
      (let [result ((first remaining-comps) arg1 arg2)]
	(if (not= 0 result)
	  result
	  (recur (rest remaining-comps)))))))

(defn compare-deps [arg1 arg2]
  (let [dep1 (dependency arg1)
        dep2 (dependency arg2)]
    (if (and (not (nil? dep1))
             (=  dep1 arg2)) ;run arg1 before arg2
      1
      (if (and (not (nil? dep2))
               (= dep2 arg1)) ;run arg2 before arg1
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

(defn compare-configuration [arg1 arg2]
  (let [conf1 (config-map (configuration arg1))
        conf2 (config-map (configuration arg2))]
    (- conf1 conf2)))

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
	    (format "Argument should be a map (of keywords to functions). Got: %s" listener))))
)

(def log-listener
     {:onTestStart (fn [result] (println (format "Starting test %s" )))
      })