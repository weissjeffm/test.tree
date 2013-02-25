(ns test.tree.watcher
  (:use [clojure.data :as data]
        [test.tree.reporter :only [configuration?]]))

(defn log-watcher [_ _ old new_]
  (println "Received event! " (second (data/diff old new_))))

(defn watch-on-pred [pred f]
  (fn [_ _ old new_]
    (let [[_ added _] (data/diff old new_)]
      (doseq [[test report] added]
        (when (pred test report)
          (f test report))))))

(defn on-fail
  "create a watcher that will call f when a test fails."
  [f]
  (watch-on-pred (fn [test report]
                   (let [r (:report report)]
                     (and r
                          (realized? r)
                          (-> r deref :result (= :fail))
                          (not (configuration? test)))))
                 f))

(defn status-watcher
  "create a watcher that will call f when a test's :status equals
   status."
  [status f]
  (fn [_ _ old new_]
    (let [[_  added _] (data/diff old new_)]
      (doseq [[test report] added]
        (when (= (:status report) status)
          (f test report))))))

(defn stdout-log-watcher
  "Prints test summaries to stdout as tests are completed."
  [k r old new_]
  (let [[_ added _] (data/diff old new_)]
    
    (doseq [[{:keys [name parameters] :as t} {:keys [status]}] added]
      (let [report (-> t new_ :report)
            parms-str (if parameters (pr-str parameters) "")]
        (if (realized? report)
          (let [report @report
                blocked-by (:blocked-by report)
                bb-str (if blocked-by (str "Blockers: " (pr-str blocked-by)) "")
                exception (or (-> report :error :throwable) "")]
            (println (apply format "%-12s %s %s   %s%s"
                            (map str [(:result report)
                                      name
                                      parms-str
                                      bb-str
                                      (.toString exception)])))))))))
