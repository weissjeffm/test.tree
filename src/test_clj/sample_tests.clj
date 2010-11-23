(ns test-clj.sample-tests
  (:use [test-clj.meta :only [gen-classes]])
  (:import [org.testng.annotations AfterClass AfterGroups AfterMethod AfterSuite AfterTest	 
	    BeforeClass BeforeGroups BeforeMethod BeforeSuite BeforeTest Test]))
					;sample tests
					;------------------------------
(declare test4)
(defn ^{:test {:configuration :beforeSuite
                 :groups #{:group1 :group2}}} 
  config1 [_]
  (do (println "running configuration1")
      (println "configuration1 complete.")))

(defn ^{:test {:groups #{:group2 :group3}}} 
  test2 [_] 
  (do(println "running test2")
     (println "test2 complete")))

(defn ^{:test {:groups #{:group1 :group2} 
                :dependsOnTests #'test2}}
  test1 [_] 
  (do (println "running test1") 
      (println "test1 complete")))

(defn ^{:test {:groups #{:group2 :group3} 
                :dependsOnTests #'test2}} 
  test3 [_] 
  (do (println "running test3") 
      (throw (RuntimeException. "test failed!")) 
      (println "test3 complete")))

(defn ^{:test {:groups #{:group1 :group3} 
                :dependsOnTests #'test3}} 
  test4 [_] 
  (do (println "running test4") 
      ;;(throw (RuntimeException. "test failed!")) 
      (println "test4 complete")))

(defn ^{:test {:groups #{:group1 :group2}
		:configuration :beforeTest}}
  config5 [_] 
  (do (println "running configuration5") 
      (println "configuration5 complete")))

(defn ^{:test {:groups #{:group1 :group2}
		:configuration :afterTest}}
  config6 [_] 
  (do (println "running configuration6") 
      (println "configuration6 complete")))

					;end sample tests
					;--------------------------------
;(gen-classes test-clj.sample-tests)

 (clojure.core/gen-class :prefix "" :name test_clj.sample_tests :methods [[^{org.testng.annotations.Test {:groups []}} test4 [] void] [^{org.testng.annotations.AfterMethod {:groups []}} config6 [] void] [^{org.testng.annotations.Test {:groups []}} test2 [] void] [^{org.testng.annotations.Test {:groups []}} test3 [] void] [^{org.testng.annotations.Test {:groups []}} test1 [] void] [^{org.testng.annotations.BeforeSuite {:groups []}} config1 [] void]])