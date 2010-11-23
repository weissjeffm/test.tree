(ns test-clj.sample-tests)
					;sample tests
					;------------------------------
(declare test4)
(defn ^{:test {:configuration :beforeSuite
                 :groups #{:group1 :group2}}} 
  config1 []
  (do (println "running configuration1")
      (println "configuration1 complete.")))

(defn ^{:test {:groups #{:group2 :group3}}} 
  test2 [] 
  (do(println "running test2")
     (println "test2 complete")))

(defn ^{:test {:groups #{:group1 :group2} 
                :dependsOnTests #'test2}}
  test1 [] 
  (do (println "running test1") 
      (println "test1 complete")))

(defn ^{:test {:groups #{:group2 :group3} 
                :dependsOnTests #'test2}} 
  test3 [] 
  (do (println "running test3") 
      (throw (RuntimeException. "test failed!")) 
      (println "test3 complete")))

(defn ^{:test {:groups #{:group1 :group3} 
                :dependsOnTests #'test3}} 
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
