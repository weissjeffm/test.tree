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

(defn skipped-tests []
  (filter (fn [t] (and (not (configuration? t))
                       (= (result t) :skip))) (keys @reports)))

(defn passed-tests []
  (filter (fn [t] (and (not (configuration? t))
                            (= (result t) :pass))) (keys @reports)))

(defn failed-tests []
  (filter (fn [n] 
            (isa? (class (result n)) Throwable)) (keys @reports)))

(defn execution-time [test]
  (let [r (report test)
        start (r :start-time)
        end (r :end-time)]
    (if (and start end)
      (/ (- end start) 1000.0)
      0)))

(defn total-time []
  (reduce + (map execution-time (keys @reports))))

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
