(ns test.tree.script
  (:use [test.tree.builder :only [data-driven]])
  (:refer-clojure :exclude [test]))

(defn split-opts [args]
  "split a list at the first item that's in an even index (zero based)
   that is not a keyword"
  (->> args
     (map-indexed vector)
     (split-with (fn [i] (or (odd? (first i))
                            (keyword? (second i)))))
     (map #(map second %))))

(defn split-deftests [args]
  (let [deftest-var (resolve 'deftest)]
    (split-with #(not= (resolve (first %)) deftest-var) args)))

(defmacro deftest [testname & options+steps]
  (let [[options steps] (split-opts options+steps)
        [steps dependent-tests] (split-deftests steps)]
    (merge `{:name ~testname
             :steps (serializable.fn/fn [] ~@steps)}
           (apply hash-map options)
           (if (not (empty? dependent-tests))
             {:more (vec dependent-tests)} {}))))

(defmacro defgroup [ & forms]
  `(do (ns ~(symbol (first forms))
         (:use test.tree.builder
               test.tree.script))
       ~@(rest forms)))

(defmacro data [argnames & rows]
  (vec rows))

(defmacro deftest-datadriven [testname & options+kw+data]
  (let [[options [kw & data]] (split-opts options+kw+data)
        thistest (merge {:name testname
                               :steps kw}
                        (apply hash-map options))]
    `(data-driven ~thistest ~(vec data))))