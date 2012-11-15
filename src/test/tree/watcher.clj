(ns test.tree.watcher
  (:use [clojure.data :as data]
        [test.tree.reporter :only [configuration?]]))

(defn log-watcher [_ _ old new]
  (println "Received event! " (second (data/diff old new))))

(defn watch-on-pred [pred f]
  (fn [_ _ old new]
    (let [[_ added _] (data/diff old new)]
      (doseq [[test report] added]
        (when (pred test report)
          (f test report))))))

(defn on-fail
  "create a watcher that will call f when a test fails."
  [f]
  (watch-on-pred (fn [test report]
                   (and (-> report :report :result (= :fail))
                        (not (configuration? test))))
                 f))

(defn status-watcher
  "create a watcher that will call f when a test's :status equals
   status."
  [status f]
  (fn [_ _ old new]
    (let [[_  added _] (data/diff old new)]
      (doseq [[test report] added]
        (when (= (:status report) status)
          (f test report))))))

(defn stdout-log-watcher
  "Prints test summaries to stdout as tests are completed."
  [k r old new]
  (let [[_ added _] (data/diff old new)]
    (doseq [[{:keys [name parameters]} {:keys [status report]}] added]
      (let [parms-str (if parameters (pr-str parameters) "")
            blocked-by (:blocked-by report)
            bb-str (if blocked-by (str "Blockers: " (pr-str blocked-by)) "")
            exception (or (-> report :error :throwable) "")]
        (if (= status :done)
          (println (apply format "%-12s %s %s   %s%s"
                          (map str [(:result report)
                                    name
                                    parms-str
                                    bb-str
                                    (.toString exception)]))))))))
