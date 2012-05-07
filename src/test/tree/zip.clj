(ns test.tree.zip
  (:require [clojure.zip :as zip]))

(defn test-zip "Create a clojure.zip structure so the tree can be
                easily walked."
  [tree]
  (zip/zipper (constantly true)
              :more 
              (fn [node children]
                (with-meta (conj node {:more (vec children)}) (meta node)))
              tree))

(defn plain-node [m]
  (dissoc m :more))

(defn nodes [z]
  (map (comp plain-node zip/node)
       (take-while #(not (zip/end? %)) (iterate zip/next z))))

(defn child-locs [z]
  (let [is-child? (fn [loc] (some #{(and loc (zip/node loc))} 
                                 (zip/children z)))]
    (->> z zip/down (iterate zip/right) (take-while is-child?))))