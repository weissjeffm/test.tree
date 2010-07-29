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

(defn dependency [test]
  (get-1stlvl-tag test :dependsOnTests))

(defn configuration [test]
  (get-1stlvl-tag test :configuration))

(defn in-group? [group myfn]
  (contains? 
   (->(meta myfn) :test :groups) 
   group))

(defn in-groups? [groups myfn]
  (some #(in-group? % myfn) groups))

(defn test? [myfn]
  (contains? (meta myfn) :test))