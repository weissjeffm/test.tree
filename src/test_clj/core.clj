(ns test-clj.core)


(def listeners (atom []))



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

;;fns to manipulate lists of tests


(defn before-tests [tests before-tests]
  (apply interleave (conj (vec (map repeat before-tests)) tests)))

(defn after-tests [tests before-tests]
  (apply interleave (conj (list* (map repeat before-tests)) tests)))

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


(defn in-dependency-chain?
  "Returns true if target is in the dependency chain of test1"
  ([all-tests test1 target] (in-dependency-chain? all-tests test1 target #{}))
  ([all-tests test1 target deps-visited]
     (let [deps1 (set (map :name (dependencies all-tests test1)))]
       ;;(println (format "Is %s a dep of %s?" (:name (meta target)) (:name (meta test1))))
       (cond 
	(empty? deps1) false 
	(some #{(:name target)} deps1)   true 
        (some deps1 deps-visited) 
        (throw (IllegalStateException. (format "%s has a cyclic dependency." (:name test1))))
	:else (some #(in-dependency-chain? all-tests % target (conj deps-visited test1))
		    deps1)))))


