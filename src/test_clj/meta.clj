(ns test-clj.meta
  (:import [org.testng.annotations AfterClass AfterGroups AfterMethod AfterSuite AfterTest	 
	    BeforeClass BeforeGroups BeforeMethod BeforeSuite BeforeTest Test]))
;;deals with test metadata structures

(def config-map 
     {:beforeSuite -3
      :beforeNS    -2
      :beforeTest  -1
      nil          0
      :afterTest   1
      :afterNS     2
      :afterSuite  3})

(defn- get-1stlvl-tag [test tag]
  (-> (meta test) :test tag))

(defn dependencies [test]
  (let [dep (get-1stlvl-tag test :dependsOnTests)]
    (cond (nil? dep)  (hash-set)
          (set? dep)  dep
	  (var? dep)  (hash-set dep)
	  true        (throw (RuntimeException.
			      (format "Test %s has a dependency that isn't a var
                                       reference to another test function, nor a set
                                       of such functions." (:name (meta test))))))))

(defn configuration [test]
  (get-1stlvl-tag test :configuration))

(defn configuration? [test]
  (not (nil? (configuration test))))

(defn parameters [test]
					;not implemented yet
  )

(defn test-name [test]
  (format "%s/%s"
	  (str (:ns (meta test)))
	  (:name (meta test))))

(defn in-group? [group myfn]
  (contains? 
   (->(meta myfn) :test :groups) 
   group))

(defn in-groups? [groups myfn]
  (some #(in-group? % myfn) groups))

(defn test? [myfn]
  (contains? (meta myfn) :test))

(def testng-map
     {:beforeSuite BeforeSuite
      :beforeNS    BeforeClass
      :beforeTest  BeforeMethod
      :afterTest   AfterMethod
      :afterNS     AfterClass
      :afterSuite  AfterSuite
      :test Test
      })

(comment (gen-class :name ^{Test {}} jeff
		    :methods [[jefftest [] void]])

	 (defmacro jeff [] `(defn ~(with-meta 'blah {:my :meta}) [] "hi")))

(defn method-name [f]
  (str (:name (meta f))))

(defn group-ann-val [t]
  (->> (meta t) :groups (map name) (into [])))

(defn as-annotation [t]
  {(if (configuration? t) (-> (configuration t) (testng-map)) Test) 
   {:groups (group-ann-val t)}})

(defmacro gen-classes [ns]
  (let [publics (vals (ns-publics ns))
	tests (filter test? publics)
	methods (map (fn [test] (let [name (method-name test)]
				  `[~(with-meta (symbol name) (as-annotation test)) [] ~'void])) tests)]
    `(gen-class :prefix "" :name ~(symbol (namespace-munge ns)) :methods [~@methods])))

