(ns test.tree.builder
  (:require [clojure.zip :as zip]
            [serializable.fn :refer [fn]]
            (test.tree [reporter :as reporter]
                       [zip :as tz]))
  (:refer-clojure :exclude [fn])
  (:import [java.io File]))

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

(defn alter-nodes-matching [pred f tree]
  (tz/tmap (fn [n] ((if (pred n) f identity) n))
        tree))

(defn data-driven "Generate a set of n data-driven tests. The first
                   argument is a template test whose :steps function
                   takes p arguments. The second argument is a n by p
                   coll of colls containing the data for the tests.
                   The metadata both the whole dataset and each row
                   will be preserved."
  [test data]
  (vec (for [item data]
         (->  test
             (merge (meta data) (meta item))
             (assoc :parameters (if (coll? item) (vec item) item))))))

(defn lazy-literal-seq [coll]
  (reduce (fn [orig form] `(lazy-seq (cons ~form ~orig))) nil (reverse coll)))

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
    (filter pred (-> z zip/root tz/test-zip tz/nodes))))

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
  (partial combine-with (fn [f g]
                          (fn [& args]
                            (try (apply f args) (finally (apply g args)))))))

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
  (fn [m]
    (map reporter/blocking-test
         ((filter-tests (every-pred (named? names)
                                    (complement (partial reporter/test-passed? (:reports m)))))
          (:test-zipper m)))))

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
  (fn [m]
    (doseq [t (tz/nodes (tz/test-zip tree))]
           (reporter/test-passed? (:reports m) t))
    []))

(defn before-all [t n]
  (let [add-child-fn (if (map? n)
                       zip/append-child
                       (fn [loc testlist] (tz/test-zip (zip/make-node loc t testlist))))]
    (-> t
      tz/test-zip
      (add-child-fn n)
      zip/root)))

(defn after-all
  "Takes a tree of tests n, and creates a new tree where
   test t runs after all the tests in n."
  [t n]
  (-> n
     tz/test-zip
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
