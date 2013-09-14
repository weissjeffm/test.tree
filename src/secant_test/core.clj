(ns secant-test.core
  (:require [slingshot.slingshot :refer [try+]]
            [loom.graph :as graph]
            [loom.alg :as graph-alg])
  (:import [java.util.concurrent LinkedBlockingQueue])
  (:refer-clojure :exclude [partial]))

(defprotocol Testable
  (as-tests [o] "Convert something into a list of tests."))

(defprotocol Invokable
  (invoke [o] "Start a procedure or function.")
  (error-check [o] "Check a procedure for obvious errors.")
  (partial [o args] "Partial application of arguments to a function."))

(defprotocol Passable
  (outcome [o] "Returns :pass :fail or :skip"))

(defrecord Test [name description steps parameters]
  Testable
  (as-tests [t] (list t))

  Invokable
  (invoke [t]
    (invoke (:steps t)))
  (error-check [t]
    (error-check (partial (:steps t)
                         (or (:parameters t) (vector)))))
  (partial [t args] (assoc t :parameters args))) ;; TODO not sure if this is right

(declare pass?)

(defrecord TestResult [test]
  Passable
  (outcome [t]
    (or (:error t)
        (and (:verifications t)
             (->> t :verifications (every? pass?)))))) ;; FIXME

(defn pass? [o]
  (= (outcome o) :pass))

(defrecord Verification [expr target-str pass?])

(def list-invokable-impl
  {:partial (fn [e lbind] `(let ~lbind
                            ~e))
   :invoke (fn [e]
             (binding [*ns* (or (-> e meta :ns) *ns*)]
               (eval e)))
   :error-check (fn [e] (eval `(do (fn [] ~e) nil)))})

(extend clojure.lang.PersistentList Invokable list-invokable-impl)
(extend clojure.lang.Cons Invokable list-invokable-impl)
(extend clojure.lang.AFn Invokable {:invoke #(.invoke %),
                                    :partial clojure.core/partial 
                                    :error-check (constantly nil)}) ; functions are already compiled, no further checking to do

(defn mktest [{:keys [name description steps parameters]}]
  (let [t (Test. name description steps parameters)]
    (error-check t)
    t))

(defmacro deftest
  "Define a test given a map, and put in a var with the given symbol."
  [sym m]
  `(def  ~(with-meta sym {:dynamic true}) (mktest ~m)))

;; Printing

;; Print clojure records with #Foopkg.fooclass{:a "bar"} style
(defmethod clojure.pprint/simple-dispatch clojure.lang.IRecord [obj] (pr obj))
(prefer-method clojure.pprint/simple-dispatch clojure.lang.IRecord clojure.lang.IPersistentMap)



;; assertions
(def ^{:dynamic true,
       :doc "var to hold assertion results, do not use directly"}
  test-verifications (atom (vector)))

(defmacro verify [x message]
  `(let [v# (Verification. (quote ~x) ~message false)
         v#  (try+
              (assert ~x ~message)
              (assoc v# :pass? true)
              (catch Object o# (assoc v# :error o#)) )]
     (swap! test-verifications conj v#)
     v#))

;; test runner wrappings

(defn run-test [{:keys [test]}]
  (invoke test))

(defn wrap-watch "Allows test execution to be watched" [runner]
  (fn [{:keys [test]}]
    ()))
(defn wrap-dependencies
  "Skips tests when their dependencies are not met"
  [runner]
  (fn [{:keys [test dependencies]}]
    (if (some ))))

(defn wrap-verifications [runner]
  (fn [{:keys [verifications :as req]}]
    (binding [test-verifications (or verifications
                                     (atom (vector)))] 
      (assoc (runner req) :verifications (deref test-verifications)))))

(defn wrap-errors [runner]
  (fn [{:keys [test] :as req}]
    (let [run-res (try+
                   (runner req)
                   (catch Object _ {:error &throw-context}))]
      (assoc run-res :test test))))

(def default-runner
  (-> run-test
      wrap-errors
      wrap-verifications))

(defn- consume
  [{:keys [runner test-graph-ref queue]}]
  (loop [t (.poll queue)]
    (and t
         (do (dosync (commute output-ref conj (runner {:test t
                                                       :dependencies (graph/incoming t)})))
             (recur (.poll queue))))))

(defn start-testrun [test-graph & [{:keys [thread-wrapper threads
                                      watchers reports-ref
                                      runner]
                               :or {thread-wrapper identity
                                    threads 1
                                    runner default-runner
                                    watchers {}}}]]
  (let [testrun-queue (LinkedBlockingQueue.)
        output-ref (or reports-ref (ref (vector)))
        traversal (graph-alg/bf-traverse test-graph)
        thread-pool (for [thread-num (range threads)]
                      (Thread. (-> consume
                                   (clojure.core/partial {:runner default-runner
                                                          :queue testrun-queue
                                                          :output-ref output-ref
                                                          :test-graph test-graph})
                                   thread-wrapper)
                               (str "secant_test-thread" thread-num)))]
    ;;queue all tests
    (doseq [t traversal]
      (.put testrun-queue t))

    ;;start worker threads
    
    (doseq [thread thread-pool]
      (.start thread))
    output-ref))


