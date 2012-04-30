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

(defmacro test [testname & options+steps]
  (let [[options steps] (split-opts options+steps)]
    (apply assoc `{:name ~testname
                   :steps `(serializable.fn/fn [] ~@steps)}
           options)))

(defmacro group [ & forms]
  `(do (ns ~(symbol (first forms))
         (:use 'test.tree.builder))
       ))

(defmacro data [argnames & rows]
  (vec rows))

(defmacro data-driven-test [testname & options+kw+data]
  (let [[options [kw & data]] (split-opts options+kw+data)
        thistest (apply assoc {:name testname
                               :steps kw}
                        options)]
    `(data-driven ~thistest ~(vec data))))