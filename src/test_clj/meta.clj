(ns test-clj.meta)
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

(defn parameters [test]
					;not implemented yet
  )

(defn in-group? [group myfn]
  (contains? 
   (->(meta myfn) :test :groups) 
   group))

(defn in-groups? [groups myfn]
  (some #(in-group? % myfn) groups))

(defn test? [myfn]
  (contains? (meta myfn) :test))