(ns test.tree.reporter
  (:require [clojure.prxml :as xml])
  (:use [clj-stacktrace.repl :only [pst-str]]))

(def reports (ref {}))

(defn report [test]
  (let [v (@reports test)]
    @(:promise v)
    (:report v)))

(defn blocked-by [test]
  (-> test report :blocked-by))

(defn result [test]
  (-> test report :result))

(defn thread [test]
  (-> test report :thread))

(defn passed? [test]
  (= (result test) :pass))

(defn configuration? [test]
  (boolean (:configuration test)))

(defn skipped? [test]
  (= (result test) :skip))

(defn skipped-tests []
  (filter (fn [t] (and (not (configuration? t))
                      (skipped? t)))
          (keys @reports)))

(defn passed-tests []
  (filter (fn [t] (and (not (configuration? t))
                            (= (result t) :pass))) (keys @reports)))

(defn failed? [t]
  (isa? (class (result t)) Throwable))

(defn failed-tests []
  (filter failed? (keys @reports)))

(defn execution-time [test]
  (let [r (report test)
        start (r :start-time)
        end (r :end-time)]
    (if (and start end)
      (/ (- end start) 1000.0)
      0)))

(defn total-time []
  (reduce + (map execution-time (keys @reports))))


(defn blocker-report []
  (->> reports
     vals
     (mapcat #(get-in % [:report :blocked-by]))
     (filter #(not (nil? %)))
     frequencies))

(defn junit-report "Produce an xml report consistent with the
                    junit report schema.  Tries to be especially
                    compatible with Jenkins and ReportNG."
  []
  (let [fails (failed-tests)
        skips (skipped-tests)
        passes (passed-tests)
        [numfail numskip numpass] (map count [fails skips passes])
        total (+ numfail numskip numpass)
        info (fn [t] {:name (let [p (:parameters t)]
                             (if p (pr-str p) (:name t)))
                     :time (execution-time t)
                     :classname (:name t)})]
    (with-out-str
      (xml/prxml [:decl! {:version "1.0"} ]
                 [:testsuite {:tests (str total)
                              :failures (str numfail)
                              :errors "0"
                              :skipped (str numskip)
                              :time (str (total-time))}
                  (concat (for [fail fails]
                            [:testcase (info fail)
                             [:failure {:type (->  fail result class .getCanonicalName )
                                        :time (execution-time fail)
                                        :message (format "On thread %s: %s"
                                                         (thread fail)
                                                         (-> fail result .getMessage))}
                              [:cdata! (-> fail result pst-str)]]])
                          (for [skip skips]
                            (let [reason (blocked-by skip)]
                              [:testcase (info skip)
                               [:skipped (if reason
                                           {:message (format "On thread %s: %s"
                                                             (thread skip)
                                                             (str reason))}
                                           {})]]))
                          (for [pass passes]
                            [:testcase (info pass)]))]))))

(defn testng-report "Produce an xml report consistent with the
                    testng report schema.  Tries to be especially
                    compatible with Jenkins."
  []
  (let [fails (failed-tests)
        skips (skipped-tests)
        passes (passed-tests)
        [numfail numskip numpass] (map count [fails skips passes])
        total (+ numfail numskip numpass)
        grouped-by-class (group-by :name (keys @reports))
        to-ms-str #(-> (* % 1000) Math/round str)
        suite-duration-ms (to-ms-str (total-time))
        info (fn [t] {:name (:name t)
                     :is-config (-> t :configuration boolean str)
                     :duration-ms (to-ms-str (execution-time t))
                     :status (cond (skipped? t) "SKIP"
                                   (passed? t) "PASS"
                                   (failed? t) "FAIL")
                     :signature (try (format "%s%s" (:name t) (-> t :steps second))
                                     (catch Exception e "sig"))})]
    (with-out-str
      (xml/prxml [:decl! {:version "1.0"} ]
                 [:testng-results {:total (str total)
                                   :failed (str numfail)
                                   :passed (str numpass)
                                   :skipped (str numskip)}
                  [:reporter-output] ;;empty
                  [:suite {:name "Test Suite"
                           :duration-ms suite-duration-ms}
                   [:test {:name "Test Tree"
                           :duration-ms suite-duration-ms}
                    (for [[class methods] grouped-by-class]
                      [:class {:name class}
                       (for [method methods]
                         (let [tr (@reports method)]
                           [:test-method (info method)
                            (when (skipped? method)
                              [:exception {:class "Skipped"}
                               [:message [:cdata! (format "Blocked by: %s"
                                                          (pr-str (get-in tr [:report :blocked-by])))]]])
                            (when (failed? method)
                              (let [e (result method)]
                                [:exception {:class (-> e .getClass str)}
                                 [:message [:cdata! (.getMessage e)]]
                                 [:full-stacktrace [:cdata! (pst-str e)]]]))
                            (if-let [params (:parameters method)] 
                              [:params (map (fn [i p] [:param {:index i}
                                                      [:value [:cdata! p]]])
                                            (iterate inc 0) params)])]))])]]]))))
