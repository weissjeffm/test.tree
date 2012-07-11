(ns test.tree.builder
  (:require [clojure.zip :as zip]
            [clojure.walk :as walk]
            [clojure.string :as str])
  (:use [test.tree.reporter :only [result passed?]]
        test.tree.zip
        slingshot.slingshot)
  (:import [java.io File]))

;;;
;;; pre-execution test manipulation functions
;;;

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

(defn data-driven "Generate a set of n data-driven tests.
                   The metadata both the whole dataset and each row
                   will be preserved."
  [test signature data]
  (vec (let [items (binding [*ns* (:ns test)]
                     (eval data))]
         (for [item items]
           (-> test
              (merge (meta data) (meta item))
              (assoc :parameters (map vector signature item)))))))

(defn dep-chain "Take a list of tests and nest them as a long tree branch"
  [tests]
  (vector (reduce #(assoc %2 :more [%1]) (reverse tests))))

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

(defn combine-with
  "combines two thunks into one, using given combinator-fn, and combine
   the serialized fn metadata, if it's present."
  [combinator-fn f g]
  (let [[sf sg] (for [i [f g]] (-> (meta i) ::source))]
    (with-meta (combinator-fn f g)
      (merge (meta f) (meta g)
             (if (and sf sg)
               {::source (concat sf (drop 2 sg))}
               {})))))

(def ^{:doc "combine two functions using juxt"}
  combine
  (partial combine-with juxt))

(def ^{:doc "combine two thunks with try/finally (where the f is the try and g is the finally."}
  combine-finally
  (partial combine-with (fn [f g] (try (f) (finally (g))))))

(defn union
  "Takes the given functions and returns a new function. When that
   function is called, it calls all the original functions and
   concatenates their results. In clojure.core terminology, this
   function would be called juxtcat."
  [& fs]
  (fn [& args]
    (apply concat (apply (apply juxt fs) args))))

(defn blocking-tests
  "Takes names of tests, returns a function, that when called, filters
   the list of names to only include tests that have failed or
   skipped."
  [& names]
  (filter-tests (every-pred (named? names) (complement passed?))))

(defn before-test "Run f before the steps of test node n" [f n]
  (let [s (:steps n)]
    (assoc n :steps (combine f s))))

(defn after-test "Run f in a finally block, after the steps of test node n" [f n]
  (let [s (:steps n)]
    (assoc n :steps (combine-finally s f))))

(defn run-before "Run f before every test that matches pred"
  [pred f tree]
  (alter-nodes-matching pred (partial before-test f) tree))

(defn run-after "Run f after every test that matches pred"
  [pred f tree]
  (alter-nodes-matching pred (partial after-test f) tree))

(defn before-each [f n]
  (run-before (complement :configuration) f n))

(defn after-each [f n]
  (run-after (complement :configuration) f n))

(defn wait-for-tree [tree]
  (fn [_]
    (doseq [t (nodes (test-zip tree))]
      (result t))
    []))

(defn before-all [t n]
  (let [add-child-fn (if (map? n)
                       zip/append-child
                       (fn [loc testlist] (test-zip (zip/make-node loc t testlist))))]
    (-> t
       test-zip
       (add-child-fn n)
       zip/root)))

(defn after-all
  "Takes a tree of tests n, and creates a new tree where
   test t runs after all the tests in n."
  [t n]
  (-> n
     test-zip
     (zip/append-child (assoc t :blockers (wait-for-tree n)))
     zip/root))

(defn read-tests [f] "Read a file that contains tests"
  (let [tests (load-file f)]
    (if (map? tests)
      (vector tests)
      tests)))

(defn from-directory [dir] "Create a tree of tests read from a directory. Each clojure source file in the directory should contain a map or multiple maps that contain tests.  The tree will be contructed with empty configuration nodes for directories, with all tests underneath.  Multiple files in the same directory will be treated as if they were one big file."
  (let [d (File. dir)
        all (vec (.listFiles d))
        dirs (filter #(.isDirectory %) all)
        files (filter #(.isFile %) all)
        clj-files (filter #(.endsWith (.getName %) ".clj") files)]
    {:name (.getName d)
     :configuration true
     :steps (constantly nil)
     :more (concat (mapcat #(read-tests (.getCanonicalPath %)) files)
                   (for [dir dirs]
                     (from-directory  (.getCanonicalPath dir))))}))
