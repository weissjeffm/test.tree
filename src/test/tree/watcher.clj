(ns test.tree.watcher
  (:use [clojure.data :as data]
        [test.tree.reporter :only [configuration?]]))

(defn log-watcher [k r o n]
  (println "Received event! " (second (data/diff o n))))

(defn watch-on-pred [pred f]
  (fn [k r old new]
    (let [[_ b _] (data/diff old new)]
      (doseq [[k v] b]
        (when (pred k v)
          (f k v))))))

(defn on-fail "create a watcher that will call f when a test fails." [f]
  (watch-on-pred (fn [t r] (and (isa? (-> r :report :result class) Throwable)
                               (not (configuration? t))))
                 f))

(defn status-watcher [stat f]
  (fn [k r old new]
    (let [[_ b _] (data/diff old new)]
      (doseq [[k v] b]
        (when (= (:status v) stat)
          (f k v))))))
