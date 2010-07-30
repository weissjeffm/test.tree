(ns test-clj.results
  (:require [test-clj.meta :as meta]))

(defn dependencies "Get the results set for all the dependencies of a test"
  [results test]
  (let [test-deps (meta/dependencies test)]
    (filter #(contains? test-deps (:fn %)) results)))

(defn lookup [results test]
  (filter #(= (:fn %) test) results))

(defn passed? [results test]
  (every? #(= (:result %) :pass) (lookup results test)))