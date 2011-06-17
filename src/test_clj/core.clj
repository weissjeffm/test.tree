(ns test-clj.core
  (:require [clojure.zip :as zip])
  (:refer-clojure :exclude [fn]))

(defmacro ^{:doc (str (:doc (meta #'clojure.core/fn))
                      "\n\n  Oh, but it also allows serialization!!!111eleven")}
  fn [& sigs]
  `(with-meta (clojure.core/fn ~@sigs)
     {:type ::serializable-fn
      ::source (quote ~&form)}))

(defmethod print-method ::serializable-fn [o ^Writer w]
  (print-method (::source (meta o)) w))

(defn test-zip [tree] (zip/zipper (constantly true)
                          #(:further-testing %)
                          #(conj %1 {:further-testing %2})
                          tree))

(defn data-driven "Generate a set of n data-driven tests from a template
                   test, a function f that takes p arguments, and a n by p list
                   of lists containing the data for the tests."
  [test f data]
  (for [item data] (assoc test
                     :procedure (with-meta (apply partial f item) (meta f))
                     :parameters item)))

(defn passed? [test]
  (= :pass (:result test)))

(defn execute-procedure "Executes test, calls listeners, returns either :pass
                    if the test exits normally,
                    :skip if a dependency failed, or an exception the test threw." 
  [test]    
  (let [start-time  (System/currentTimeMillis)]
    (assoc test
      :result (try ((:procedure test))             ;test fn is called here 
                   :pass
                   (catch Exception e e)) 
      :start-time start-time
      :end-time (System/currentTimeMillis))))


(defn run-test [unrun-test-tree]
  (let [this-test (zip/node unrun-test-tree)
        dependency (zip/up unrun-test-tree)
        dep-passed? (if dependency
                      (passed? (zip/node dependency))
                      true)]
    (zip/replace unrun-test-tree
                 (if dep-passed? (execute-procedure this-test)
                     (assoc this-test :result :skip)))))

(defn run-all [unrun-test-tree]
  (first (drop-while (fn [n] (not (:result (zip/node n))))
               (iterate (comp zip/next run-test) unrun-test-tree))))


