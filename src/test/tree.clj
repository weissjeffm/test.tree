(ns test.tree
  (:require [clojure.pprint :as pprint]
            [clojure.set :as sets]
            [test.assert :refer [is]]))

(defrecord Test [name steps more description parameters
                 configuration blockers line file])

;; Construction and validation

(defn unrecognized-keys
  "Any record keys that aren't part of the original record def"
  [m]
  (let [ksyms (->> m keys (map (comp symbol name)) set)]
    (->> (Test/getBasis)
       set
       (sets/difference ksyms))))

(def maybe-sequential? (some-fn nil? sequential?))
(def maybe-fn? (some-fn nil? fn?))
(def maybe-sequential-or-fn? (some-fn nil? sequential? fn?))

(defn validate-test [m]
  (is (empty? (unrecognized-keys m)))
  (is (fn? (:steps m)))
  (is (string? (:name m)))
  (is (maybe-sequential? (:more m)))
  (is (maybe-sequential-or-fn? (:parameters m)))
  (is (maybe-fn? (:blockers m)))
  true)

(defn new-test
  "Creates a new test from a map, with validation"
  [m]
  {:pre [(validate-test m)]}
  (map->Test m))

(defn blocking-test
  "Returns a representation of a test suitable for listing as a
   blocker of another test. Just the name and parameters."
  [t]
  (select-keys t [:name :parameters]))

;; Depending on the value of this var, either print the standard
;; clojure way, or turn all objects that can't be printed readably
;; into strings that can at least be read in.

(def ^:dynamic *print-all-readably* nil)

;; save the original core dispatch functions
(defonce print-method-orig-object (get-method print-method Object))
(defonce print-method-orig-ideref (get-method print-method clojure.lang.IDeref))

(defn print-quoted [f o w]
  (clojure.lang.RT/print (str "#<" o ">") w))

(defn print-ideref [f o w]
  (clojure.lang.RT/print (str
                          (if (and (instance? clojure.lang.IPending o) (not (.isRealized o)))
                                            :pending
                                            @o)) w))

(defmethod print-method Test [o w]
  (print-method (into {} o) w))

(defmethod print-method clojure.lang.IDeref [o ^java.io.Writer w]
  (if *print-all-readably*
    (print-ideref print-method-orig-ideref o w)
    (print-method-orig-ideref o w)))

(defmethod print-method Object [o, ^java.io.Writer w]
  (if *print-all-readably*
    (print-quoted print-method-orig-object o w)
    (print-method-orig-object o w)))


