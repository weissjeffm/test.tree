(ns test-clj.results
  (:require [test-clj.meta :as meta]))



(defn lookup [results test]
  (filter #(= (:fn %) test) results))

(defn passed? [results test]
  (every? #(= (:result %) :pass) (lookup results test)))

(defn summary [results]
  (let [numtests (count results)
	configpred #(meta/configuration (:fn %))
	tests (remove configpred results)
	configs (filter configpred results)
	countbyres (fn [pred] (count (filter pred tests)))
	numpassed (countbyres #(= (:result %) :pass))
	numskipped (countbyres #(= (:result %) :skip))
	numfailed (countbyres #(instance? Exception (:result %)))
	avgexec (reduce + (map (fn [result] (- (:endTime result) (:startTime result))) results))]
    {:passed numpassed
     :failed numfailed
     :skipped numskipped
     :avgExecTimeMs avgexec}))
