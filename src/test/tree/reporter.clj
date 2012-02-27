(ns test.tree.reporter
  (:require [clojure.prxml :as xml])
  (:use clojure.pprint
        [clj-stacktrace.repl :only [pst-str]]))

(def reports (ref {}))

(defmulti exception :wrapper)
(defmethod exception nil [e] (:object e))
(defmethod exception :default [e] (:wrapper e))

(defn report [test]
  (let [v (@reports test)]
    @(:promise v)
    (:report v)))

(defn blocked-by [test]
  (-> test report :blocked-by))

(defn result [test]
  (-> test report :result))

(defn error [test]
  (-> test report :error))

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
  (= (result t) :fail))

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

(defn- format-exception-msg [t]
  (format "On thread %s: %s"
          (thread t)
          (-> t error :message))) 

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
    (binding [xml/*prxml-indent* 2]
      (xml/prxml [:decl! {:version "1.0"} ]
                 [:testsuite {:tests (str total)
                              :failures (str numfail)
                              :errors "0"
                              :skipped (str numskip)
                              :time (str (total-time))}
                  (concat (for [fail fails]
                            [:testcase (info fail)
                             (let [err (error fail)
                                   obj (:object err)]
                               [:failure {:type (or (:type obj)
                                                    (-> obj .getClass .getName))
                                          :time (execution-time fail)
                                          :message (format-exception-msg fail)}
                                [:cdata! (-> fail error exception pst-str)]])])
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

(def testng-dateformat (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss'Z'"))

(defn syntax-highlighter [sh-url]
  (fn [s]
    (let [url #(str sh-url %)]
      [[:script {:type "text/javascript" :src (url "scripts/shCore.js")} " "]
       [:script {:type "text/javascript" :src (url "scripts/shBrushClojure.js")} " "]
       [:link {:href (url "styles/shCore.css")}]
       [:link {:href (url "styles/shCoreEmacs.css")}]
       [:script {:type "text/javascript"} "SyntaxHighlighter.all()"]
       [:pre {:class "brush: clj;gutter: false; toolbar: false"} s]])))

(defn plain-text [s]
  [[:cdata! s]])

;;a rebindable function to do syntax highlighting on some code/data in
;;a report.  Should take text and return the syntaxhighlighted html
(def ^:dynamic syntax-highlight plain-text)


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
        date-format (fn [unixdate] (.format testng-dateformat (java.util.Date. unixdate)))
        info (fn [t tr] (merge {:name (:name t)
                               :duration-ms (to-ms-str (execution-time t))
                               :status (cond (skipped? t) "SKIP"
                                             (passed? t) "PASS"
                                             (failed? t) "FAIL")
                               :signature (try (format "%s%s" (:name t) (-> t :steps second))
                                               (catch Exception e "sig"))
                               :started-at (date-format (:start-time tr))
                               :finished-at (date-format (:end-time tr))
                               :description (or (:description t) "")}
                              (when (:configuration t) {:is-config "true"})))]
   
    (binding [xml/*prxml-indent* 2]
      (xml/prxml [:decl! {:version "1.0"} ]
                 [:testng-results {:total (str total)
                                   :failed (str numfail)
                                   :passed (str numpass)
                                   :skipped (str numskip)}
                  [:reporter-output] ;;empty
                  [:suite {:name "Test Suite"
                           :duration-ms suite-duration-ms}
                   [:test {:name "Test Tree"
                           :duration-ms suite-duration-ms
                           :started-at (date-format (System/currentTimeMillis))
                           :finished-at (date-format (System/currentTimeMillis))} ;;need real values here
                    (for [[clazz methods] grouped-by-class]
                      [:class {:name clazz}
                       (for [method methods]
                         (let [tr (:report (@reports method))]
                           [:test-method (info method tr)
                            (when (skipped? method)
                              [:exception {:class "Skipped"}
                               [:message [:cdata! (format "Blocked by: %s"
                                                          (pr-str (:blocked-by tr)))]]
                               [:short-stacktrace [:cdata! "Skips are not errors, no stacktrace."]]])
                            (when (failed? method)
                              (let [err (-> method error (dissoc :stack-trace)) 
                                    e (exception err)
                                    msg (format-exception-msg method)
                                    pretty-st (pst-str e)
                                    not-empty (fn [s] (and s (-> s .length (> 0))))]
                                (when (every? not-empty [msg pretty-st])
                                  [:exception {:class (-> e .getClass str)}
                                   [:message [:cdata! msg]]
                                   [:short-stacktrace [:cdata! pretty-st]]
                                   (vec
                                    (concat [:full-stacktrace] (syntax-highlight
                                                                (with-out-str
                                                                  (pprint err)))))])))
                            (if-let [params (:parameters tr)] 
                              [:params (map (fn [i p] [:param {:index i}
                                                      [:value [:cdata! (pr-str p)]]])
                                            (iterate inc 0) params)])]))])]]]))))