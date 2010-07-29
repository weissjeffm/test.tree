(ns test-clj.meta)
;;deals with test metadata structures

(defn- get-1stlvl-tag [tag]
  (-> (meta test) :test tag))

(defn dependency [test]
  (get-1stlvl-tag :dependsOnTests))

(defn configuration [test]
  (get-1stlvl-tag :configuration))

(defn in-group? [group myfn]
  (contains? 
   (->(meta myfn) :test :groups) 
   group))

(defn in-groups? [groups myfn]
  (some #(in-group? % myfn) groups))

(defn test? [myfn]
  (contains? (meta myfn) :test))