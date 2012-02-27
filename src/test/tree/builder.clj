(ns test.tree.builder
  (:require [clojure.zip :as zip])
  (:use [serializable.fn :only [fn]])
  (:refer-clojure :exclude [fn]))

;;;
;;; pre-execution test manipulation functions
;;;

(defprotocol Delay
  (realize [d]))

(extend-protocol Delay
  clojure.lang.Fn
  (realize [t] (.call t))

  clojure.lang.Delay
  (realize [t] (clojure.core/force t))

  java.lang.Object
  (realize [t] t))

(defn test-zip "Create a clojure.zip structure so the tree can be
                easily walked."
  [tree]
  (zip/zipper (constantly true)
              :more 
              (fn [node children]
                (with-meta (conj node {:more children}) (meta node)))
              tree))

(defn tmap "Does a depth-first walk of the tree, passes each node thru
            f, and returns the tree"
  [f tree]
  (let [walk-fn (fn [l] (let [new-l (zip/edit l f)] 
                         (zip/next new-l)))]
    (->> tree
       test-zip
       (iterate walk-fn)
       (drop-while (complement zip/end?))
       first
       zip/root)))

(defn alter-nodes-matching [pred f tree]
  (tmap (fn [n] ((if (pred n) f identity) n))
        tree))

(defn plain-node [m]
  (dissoc m :more))

(defn child-locs [z]
  (let [is-child? (fn [loc] (some #{(and loc (zip/node loc))} 
                                 (zip/children z)))]
    (->> z zip/down (iterate zip/right) (take-while is-child?))))

(defn data-driven "Generate a set of n data-driven tests from a
                   template test, a function f that takes p arguments,
                   and a n by p coll of colls containing the data for
                   the tests. The metadata on either the overall set,
                   or rows of data, will be extracted and merged with
                   the tests"
  [test data]
  (for [item data]
    (->  test
        (merge (meta data) (meta item))
        (assoc :parameters item))))

(defn dep-chain "Take a list of tests and nest them as a long tree branch"
  [tests]
  (vector (reduce #(assoc %2 :more [%1]) (reverse tests))))

(defn nodes [z]
  (map (comp plain-node zip/node)
       (take-while #(not (zip/end? %)) (iterate zip/next z))))

(defn by-key [k vals]
  (fn [n]
    (if n (some (set vals) [(n k)]))))

(defn named?
  [testnames]
   (by-key :name testnames))

(defn tagged?
  [testtags]
  (by-key :tags testtags))

(defn filter-tests [pred]
  (fn [z]
    (filter pred (-> z zip/root test-zip nodes))))

(defn combine "combines two thunks into one, using juxt"
  [f g]
  (let [[sf sg] (for [i [f g]] (-> (meta i) ::source))]
    (with-meta (juxt f g)
      (merge (meta f) (meta g)
             (if (and sf sg)
               {::source (concat sf (drop 2 sg))}
               {})))))

(defn juxtcat [& fs]
  (fn [& args]
    (apply concat (apply (apply juxt fs) args))))

(defn before-test "Run f before the steps of test node n" [f n]
  (let [s (:steps n)]
    (assoc n :steps (combine f s))))

(defn after-test "Run f after the steps of test node n" [f n]
  (let [s (:steps n)]
    (assoc n :steps (combine s f))))

(defn run-before "Run f before every test that matches pred"
  [pred f tree]
  (alter-nodes-matching pred (partial before-test f) tree))

(defn before-all [f n]
  (run-before (complement :configuration) f n))
