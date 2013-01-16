(ns test.tree.zip
  (:require [test.tree :as tree]
            [clojure.zip :as zip]))

(defn test-zip "Create a clojure.zip structure so the tree can be
                easily walked. Also validates the tree structure."
  [tree]
  (zip/zipper (constantly true)
              :more 
              (fn [node children]
                (with-meta (conj node {:more (vec children)}) (meta node)))
              tree))

(defn tmap "Does a depth-first walk of the tree, passes each node thru
            f, and returns the tree"
  [f tree]
  (let [walk-fn #(-> % (zip/edit f) zip/next)]
    (->> tree
       test-zip
       (iterate walk-fn)
       (drop-while (complement zip/end?))
       first
       zip/root)))

(defn validate [tree]
  (tmap tree/new-test tree))

(defn plain-node [m]
  (dissoc m :more))

(defn nodes [z]
  (map (comp plain-node zip/node)
       (take-while #(not (zip/end? %)) (iterate zip/next z))))

(defn child-locs [z]
  (let [is-child? (fn [loc] (some #{(and loc (zip/node loc))} 
                                 (zip/children z)))]
    (->> z zip/down (iterate zip/right) (take-while is-child?))))