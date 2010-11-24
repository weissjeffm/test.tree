(ns test-clj.testng
  (:use [test-clj.meta :only [test? configuration get-1stlvl-tag]])
  (:import [org.testng.annotations AfterClass AfterGroups AfterMethod AfterSuite AfterTest	 
	    BeforeClass BeforeGroups BeforeMethod BeforeSuite BeforeTest Test]))

(def testng-map
     {:beforeSuite BeforeSuite
      :beforeNS    BeforeClass
      :beforeTest  BeforeMethod
      :afterTest   AfterMethod
      :afterNS     AfterClass
      :afterSuite  AfterSuite
      :test Test
      :dependsOnTests :dependsOnMethods
      })

(defn method-name [t]
  (str (:name (meta t))))

(defn group-ann-val [t]
  (->> (get-1stlvl-tag t :groups) (map name) (into [])))

(defn as-annotation [t]
  (let [test-type (or (configuration t) :test)
	annotation-symbol  (-> (testng-map test-type) .getName symbol)]
   {annotation-symbol {:groups (group-ann-val t)}}))

(defmacro gen-class-testng []
  (let [publics (vals (ns-publics *ns*))
	tests (filter test? publics)
	methods (map (fn [test] (let [name (method-name test)]
				  `[~(with-meta (symbol name) (as-annotation test)) [] ~'void])) tests)]
    `(gen-class :prefix "" :name ~(symbol (namespace-munge *ns*)) :methods [~@methods])))
