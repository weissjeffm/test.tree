(ns secant-test.core
  (:require [loom.graph :as graph]
            [loom.attr :as attr]
            loom.io
            [loom.alg :as graph-alg]
            [slingshot.slingshot :refer [try+]]
            [seesaw.core :as seesaw])
  (:import [com.mxgraph.view mxGraph ]
           [com.mxgraph.swing mxGraphComponent]))

(defprotocol Testable
  (as-tests [o] "Convert something into a list of tests."))

(defprotocol Invokable
  (invoke [o] "Start a procedure or function.")
  (error-check [o] "Check a procedure for obvious errors.")
  (papply [o args] "Partial application of arguments to a function."))

(defprotocol Queueable
  (queue [o] [o start] [o start f]
    "Add an object to a queue (maybe adding multiple items), starting with start value in object."))

(defrecord Test [name description steps parameters]
  Testable
  (as-tests [o] (list o))

  Invokable
  (invoke [this]
    (invoke (:steps this)))
  (error-check [this]
    (error-check (papply (:steps this)
                         (or (:parameters this) (vector)))))
  (papply [o args] (assoc o :parameters args)))


(extend-protocol Invokable
  clojure.lang.AFn
  (invoke [f] (.invoke f))
  (papply [f args] (apply partial f args))
  (error-check [f] nil) ;;functions are already compiled, no further checking to do

  clojure.lang.PersistentList
  (papply [e lbind] `(let ~lbind
                       ~e))
  (invoke [e]
    (binding [*ns* (or (-> e meta :ns) *ns*)]
      (eval e)))
  (error-check [e] (eval `(do (fn [] ~e) nil)))

  clojure.lang.Cons 
  (papply [e lbind] `(let ~lbind
                       ~e))
  (invoke [e]
    (binding [*ns* (or (-> e meta :ns) *ns*)]
      (eval e)))
  (error-check [e] (eval `(do (fn [] ~e) nil))))

(extend-protocol Queueable
  loom.graph.SimpleDigraph
  (queue
    ([g] (graph-alg/bf-traverse g))
    ([g s] (graph-alg/bf-traverse g s))
    ([g s f] (graph-alg/bf-traverse g s :f f))))


(defn mktest [{:keys [name description steps parameters]}]
  (let [t (Test. name description steps parameters)]
    (error-check t)
    t))

(defmacro deftest [sym m]
  `(def  ~(with-meta sym {:dynamic true}) (mktest ~m)))

(def all-dd-tests
  {:t1 {:name "t1", :steps '(println "t1"), :description "test foo"}
   :t2 {:name "t2", :steps '(println "t2"), :description "test foo"}
   :t3 {:name "t3", :steps '(println "t3"), :description "test foo"}})

(deftest login {:name "login", :steps '(println "login"), :description "test foo"})
(deftest create-org {:name "create-org", :steps '(println "create-org"), :description "test foo"})
(deftest create-prod {:name "create-prod", :steps '(println "create-prod"), :description "test foo"})
(deftest create-env {:name "create-env", :steps '(println "create-env"), :description "test foo"})
(deftest promo-empty-prod {:name "promo-empty-prod", :steps '(println "promo-empty-prod"), :description "test foo"})
(deftest promo-content {:name "promo-content", :steps '(println "promo-content"), :description "test foo"})
(deftest create-repo {:name "create-repo", :steps '(println "create-repo"), :description "test foo"})
(deftest sync-repo {:name "sync-repo", :steps '(println "sync-repo"), :description "test foo"})

(defmacro with-bindings-fn "passes all vars in coll thru f and binds the var to new value"
  [f coll & body]
  `(with-bindings ~(zipmap coll (for [i coll] `(~f (deref ~i))))
     ~@body))

(def test-structure
  (list
   [:bz-login login]
   {login [create-org create-prod]}
   {create-org (conj (vals all-dd-tests) create-env)}
   {create-env [promo-empty-prod promo-content]}
   [create-prod create-repo]
   {create-repo [promo-empty-prod sync-repo]}
   [:bz-sync sync-repo]
   [sync-repo promo-content]))

(defn gmap "Walk all the edges ")

(def mytests2
  (-> (apply graph/digraph
       test-structure)
      (attr/add-attr :bz-login :shape :rectangle)
      (attr/add-attr :bz-sync :shape :rectangle)))




(def testexpr '(+ x y z))


;; test runner wrappings
(defn run-test [test]
  (invoke test))

(defn wrap-failures [runner]
  (fn [req]
    (try+
     {:returned (run-test (:test req))
      :outcome :pass}
     (catch Object _ {:outcome :fail
                      :error &throw-context}))))

(defn run [tests run-params]
  ((-> run-test
       wrap-failures)
   tests (hash-map)))

(comment (loom.io/view mytests2 :node-label :name))
