(ns test-clj.core
  (:require [clojure.zip :as zip]
            [clojure.pprint :as pprint]
            [clojure.contrib.prxml :as xml])
  (:use [clojure.contrib.core :only [-?>]])
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
                                  #(:more %)
                                  (fn [node children]
                                    (with-meta (conj node {:more children}) (meta node)))
                                    tree))

(defn walk-all "Does a depth-first walk of the tree, for each node, passes the loc thru f, and returns the tree" [tree f]
  (first (drop-while (complement zip/end?)
                     (iterate (fn [l]
                                (let [new-l (zip/edit l f)] 
                                  (zip/next new-l))) tree))))

(defn passed? [test]
  (= :pass (:result test)))

(defn configuration? [test]
  (boolean (:configuration test)))

(defn execute [name proc]
  (proc))

(defn execute-procedure "Executes test, calls listeners, returns either :pass
                    if the test exits normally,
                    :skip if a dependency failed, or an exception the test threw." 
  [test]    
  (let [start-time  (System/currentTimeMillis)]
    (assoc test
      :result (try (execute (:name test)
                            (:steps test))             ;test fn is called here
                   :pass
                   (catch Exception e e)) 
      :start-time start-time
      :end-time (System/currentTimeMillis))))



(defn run-test [unrun-test-tree]
  (let [this-test (zip/node unrun-test-tree)
        direct-dep (zip/up unrun-test-tree)
        dd-passed? (if direct-dep
                     (passed? (zip/node direct-dep))
                     true)
        failed-pre ((or (:pre-fn this-test) (constantly nil)) (zip/root unrun-test-tree))
        deps-passed? (and dd-passed? (not failed-pre))]
    (zip/replace unrun-test-tree
                 (if deps-passed? (execute-procedure this-test)
                     (assoc this-test
                       :result :skip
                       :failed-pre failed-pre)))))

(defn run-all [unrun-test-tree]
  (first (drop-while (fn [n] (not (:result (zip/node n))))
               (iterate (comp zip/next run-test) unrun-test-tree))))

(defn data-driven "Generate a set of n data-driven tests from a template
                   test, a function f that takes p arguments, and a n by p list
                   of lists containing the data for the tests."
  [test f data]
  (for [item data] (assoc test
                     :steps (with-meta (apply partial f item) (meta f))
                     :parameters item)))

(defn plain-node [n]
  (dissoc n :more))

(defn nodes [z]
  (map (comp plain-node zip/node) (take-while #(not (zip/end? %)) (iterate zip/next z))))
;;helper functions

(defn by-field [k vals]
  (fn [n]
    (if n (some (set vals) [(n k)]))))

(defn by-name
  [tests]
   (by-field :name tests))

(defn unsatisfied [pred]
  (fn [rootnode] (let [unsat (filter #(and ((complement passed?) %1)
                                          (pred %1))
                                    (nodes (test-zip rootnode)))]
                  (if (= 0 (count unsat)) nil
                      unsat))))

(defn filter-tests [z p]
  (filter p (nodes z)))

(defn skipped-tests [z]
  (filter-tests z (fn [n]
                  (and (not (configuration? n))
                       (= (:result n) :skip)))))

(defn passed-tests [z]
  (filter-tests z (fn [n] (and (not (configuration? n))
                            (= (:result n) :pass)))))

(defn failed-tests [z]
  (filter-tests z (fn [n] (and (not (configuration? n))
                             (isa? (class (:result n)) Exception)))))

(defn execution-time [n]
  (let [start (:start-time n)
        end (:end-time n)]
    (if (and start end)
      (/ (- end start) 1000.0)
      0)))

(defn total-time [z]
  (reduce + (map execution-time (nodes z))))

(defn junit-report [z]
  (let [fails (failed-tests z)
        skips (skipped-tests z)
        passes (passed-tests z)
        all (nodes z)
        info (fn [n] {:name (or (:parameters n) (:name n))
                     :time (execution-time n)
                     :classname (:name n)})]
    (with-out-str
      (xml/prxml [:decl! {:version "1.0"} ]
                 [:testsuite {:tests (str (count all))
                              :failures (str (count fails))
                              :errors "0"
                              :skipped (str (count skips))
                              :time (str (total-time z))}
                  (concat (for [fail fails]
                            [:testcase (info fail)
                             [:failure {:type (-> (:result fail) class .getCanonicalName )
                                        :time (execution-time fail)}]])
                          (for [skip skips]
                            [:testcase (info skip)
                             [:skipped]])
                          (for [pass passes]
                            [:testcase (info pass)]))]))))

(defn run-suite [tests]
  (let [result (run-all (test-zip tests))
        fresh-result (-> result zip/root test-zip)]
    (pprint/pprint result)
    (spit "junitreport.xml" (junit-report fresh-result))
    (zip/root fresh-result)))
