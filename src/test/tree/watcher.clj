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
  (watch-on-pred (fn [t r] (and (-> r :report :result (= :fail))
                               (not (configuration? t))))
                 f))

(defn status-watcher [stat f]
  (fn [k r old new]
    (let [[_ b _] (data/diff old new)]
      (doseq [[k v] b]
        (when (= (:status v) stat)
          (f k v))))))

(defn stdout-log-watcher [k r o n]
  (let [[_ d _] (data/diff o n)]
    (doseq [[{:keys [name parameters]} {:keys [status report]}] d]
      (let [parms-str (if parameters (pr-str parameters) "")]
        (if (= status :done)
          (println (apply format "%-12s %s %s\n" (map str [(:result report) name parms-str]))))))))
