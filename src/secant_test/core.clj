(ns secant-test.core
  (:require [loom.graph :as graph]
            [loom.attr :as attr]
            loom.io
            [loom.alg :as graph-alg]
            [slingshot.slingshot :refer [try+]])
  (:import [java.util.concurrent LinkedBlockingQueue])
  (:refer-clojure :exclude [partial]))

(defprotocol Testable
  (as-tests [o] "Convert something into a list of tests."))

(defprotocol Invokable
  (invoke [o] "Start a procedure or function.")
  (error-check [o] "Check a procedure for obvious errors.")
  (partial [o args] "Partial application of arguments to a function."))

(defrecord Test [name description steps parameters]
  Testable
  (as-tests [o] (list o))

  Invokable
  (invoke [this]
    (invoke (:steps this)))
  (error-check [this]
    (error-check (partial (:steps this)
                         (or (:parameters this) (vector)))))
  (partial [o args] (assoc o :parameters args)))

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


;; assertions
(def ^{:dynamic true, :doc "var to hold assertion results, do not use directly"}
  test-assertions nil)

(defmacro assert* [x message]
  `(let [v# (Verification. (quote ~x) ~message false)
         v#  (try+
              (assert ~x ~message)
              (assoc v# :pass? true)
              (catch Object o# (assoc v# :error o#)) )]
     (swap! test-assertions conj v#)
     v#))

(defmacro with-assertions
  "Collect assertions (called with assert), and return them as a list at the end of body"
  [& body]
  `(binding [test-assertions (atom (vector))]
     ~@body
     test-assertions))


(defmacro defmytest [sym name & body]
  `(deftest ~sym {:name ~name :steps (quote (with-assertions ~@body))}))

;; test runner wrappings

(defn run-test [{:keys [test]}]
  (invoke test))

(defn wrap-assertions [runner]
  (fn [req]
    (with-assertions 
      (runner req))))

(defn wrap-failures [runner]
  (fn [req]
    (try+
     {:returned (run-test (:test req))
      :outcome :pass}
     (catch Object _ {:outcome :fail
                      :error &throw-context}))))

(def default-runner
  (-> run-test
      wrap-failures))

(defn consume [runner q]
  (loop [t (.poll q)]
    (and t (do (runner {:test t})
               (recur (.poll q))))))

(defn start-testrun [tests & [{:keys [thread-wrapper threads
                                      watchers reports-ref
                                      runner]
                               :or {thread-wrapper identity
                                    threads 1
                                    runner default-runner
                                    watchers {}}}]]
  (let [testrun-queue (LinkedBlockingQueue.)
        thread-pool (for [thread-num (range threads)]
                      (Thread. (-> (partial consume testrun-queue)
                                   thread-wrapper)
                               (str "secant_test-thread" thread-num)))]
    ;;queue all tests
    (doseq [t tests]
      (.put testrun-queue t))

    ;;start worker threads
    (doseq [thread thread-pool] (.start thread))))

(comment (loom.io/view mytests2 :node-label :name))
(comment (map :name (graph-alg/bf-traverse mytests2)) )
