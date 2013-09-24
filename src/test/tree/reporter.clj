(ns test.tree.reporter
  (:require [hickory.core :as html]
            [clojure.data.xml :as xml]
            [clojure.set :as sets]
            test.tree.zip)
  (:use clojure.pprint 
        [clj-stacktrace.repl :only [pst-str]]))

(defn init-reports [z]
  (zipmap (test.tree.zip/nodes z)
          (repeatedly (fn [] {:status :waiting
                              :lock (promise)}))))

(defmulti exception :wrapper)
(defmethod exception nil [e] (:object e))
(defmethod exception :default [e] (:wrapper e))


;; Functions to process testsentries (mapentry of test to report)

(def test-report (comp :report val))

(def blocked-by (comp :blocked-by test-report))

(def result (comp :result test-report))

(def error (comp :error test-report))

(def thread (comp :thread test-report))

(def realized-parameters (comp :parameters test-report))

(def configuration? (comp boolean :configuration key))

(def testgroup (comp :groups key))


(defn passed? [testentry]
  (= (result testentry) :pass))

(defn skipped? [testentry]
  (= (result testentry) :skip))

(defn failed? [testentry]
  (= (result testentry) :fail))

(defn execution-time [testentry]
  (if-let [r (test-report testentry)]
    (let [start (r :start-time)
          end (r :end-time)]
      (if (and start end)
        (/ (- end start) 1000.0)
        0))
    0))

;; Other functions

(defn test-passed?
  "Given a reference to reports, wait for test to complete (if not
  already) and return whether it passed."
  [report-ref test]
  (-> report-ref deref (get test) :report :result (= :pass)))

(defn total-time [report]
  (reduce + (map execution-time report)))

(defn blocking-test
  "Returns a representation of a test suitable for listing as a
   blocker of another test. Just the name and parameters."
  [t]
  (select-keys t [:name :parameters]))

(defn blocker-report [report]
  (->> report
     vals
     (mapcat #(-> % :report :blocked-by))
     (filter (complement nil?))
     frequencies))

(defn- format-exception-msg [t]
  (format "On thread %s: %s"
          (thread t)
          (-> t error :message))) 

;; Functions for generating specific format of report

(defn junit-report "Produce an xml report consistent with the
                    junit report schema.  Tries to be especially
                    compatible with Jenkins and ReportNG."
  [report]
  (let [by-result (group-by result report)
        fails (by-result :fail)
        skips (by-result :skip)
        passes (by-result :pass)
        [numfail numskip numpass] (map count [fails skips passes])
        total (+ numfail numskip numpass)
        info (fn [[t :as e]]
               {:name (let [p (:parameters t)]
                        (if p (pr-str p) (:name t)))
                :time (execution-time e)
                :classname (:name t)})]
    (xml/emit-str
     (xml/sexp-as-element
      [:testsuite {:tests (str total)
                   :failures (str numfail)
                   :errors "0"
                   :skipped (str numskip)
                   :time (str (total-time report))}
       (concat (for [fail fails]
                 [:testcase (info fail)
                  (let [err (error fail)
                        obj (:object err)]
                    [:failure {:type (or (:type obj)
                                         (-> obj .getClass .getName))
                               :time (execution-time fail)
                               :message (format-exception-msg fail)}
                     [:-cdata (-> fail error exception pst-str)]])])
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
      (str (->>
            [{:type :element :tag :script :attrs {:type "text/javascript" :src (url "scripts/shCore.js")}}
             {:type :element :tag :script :attrs {:type "text/javascript" :src (url "scripts/shBrushClojure.js")}}
             {:type :element :tag :link :attrs {:href (url "styles/shCore.css"), :rel "stylesheet"}}
             {:type :element :tag :link :attrs {:href (url "styles/shCoreEmacs.css"), :rel "stylesheet"}}
             {:type :element :tag :script :attrs {:type "text/javascript"} :content ["SyntaxHighlighter.all()"]}
             {:type :element :tag :pre :attrs {:class "brush: clj;gutter: false; toolbar: false"} :content [s]}]
            (map html/hickory-to-html)
            (apply str))))))

(defn pre [text]
  (html/hickory-to-html {:type :element, :tag :pre, :content [text]}))

;;a rebindable function to do syntax highlighting on some code/data in
;;a report.  Should take text and return the syntaxhighlighted html
(def ^:dynamic syntax-highlight identity)


(defn pst-str-with-fallback
  "Try to pretty print stacktrace with clj-stacktrace but it's buggy so fall back to standard"[e]
  (try (clj-stacktrace.repl/pst-str e)
       (catch Exception ex
         (let [sw (java.io.StringWriter.)]
           (.printStackTrace e (java.io.PrintWriter. sw))
           (.toString sw)))))

(defn testng-report "Produce an xml report consistent with the
                    testng report schema.  Tries to be especially
                    compatible with Jenkins."
  [report]
  (let [by-result (group-by result report)
        fails (by-result :fail)
        skips (by-result :skip)
        passes (by-result :pass)
        [numfail numskip numpass] (map count [fails skips passes])
        total (+ numfail numskip numpass)
        by-class (group-by testgroup report)
        to-ms-str #(-> (* % 1000) Math/round str)
        suite-duration-ms (to-ms-str (total-time report))
        date-format (fn [unixdate] (.format testng-dateformat (java.util.Date. unixdate)))
        info (fn [[t tr :as e]] 
               (merge {:name (:name t)
                       :duration-ms (to-ms-str (execution-time e))
                       :status (cond (skipped? e) "SKIP"
                                     (passed? e) "PASS"
                                     (failed? e) "FAIL")
                       :signature (try (format "%s%s" (:name t) (-> t :steps second))
                                       (catch Exception _ "sig"))
                       :started-at (date-format (-> tr :report :start-time))
                       :finished-at (date-format (-> tr :report :end-time))
                       :description (or (:description t) "")
                       :uuid (or (:uuid t) "")}
                      (when (:configuration t) {:is-config "true"})))]   
    (xml/emit-str
     (xml/sexp-as-element [:testng-results {:total (str total)
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
                             (for [[groups method-entries] by-class]
                               [:class {:name (apply str (interpose "."
                                                                    (or
                                                                     (-> groups reverse vec (conj (first groups)))
                                                                     ["rootClass"])))}
                                (for [method-entry method-entries]
                                  [:test-method (info method-entry)
                                   (when (skipped? method-entry)
                                     [:exception {:class "Skipped"}
                                      [:message [:-cdata (format "Blocked by: %s"
                                                                 (-> method-entry blocked-by pr-str))]]
                                      [:short-stacktrace [:-cdata "Skips are not errors, no stacktrace."]]])
                                   (when (failed? method-entry)
                                     (let [err (-> method-entry error (dissoc :stack-trace)) 
                                           e (exception err)
                                           msg (format-exception-msg method-entry)
                                           pretty-st (pst-str-with-fallback e)
                                           not-empty (fn [s] (and s (-> s .length (> 0))))]
                                       (when (every? not-empty [msg pretty-st])
                                         [:exception {:class (-> e .getClass str)}
                                          [:message [:-cdata
                                                     (str (with-open [sw (java.io.StringWriter.)]
                                                            (pprint err sw)
                                                            (-> sw .toString syntax-highlight))
                                                          (pre pretty-st))]]
                                          [:short-stacktrace [:-cdata pretty-st]]])))
                                   (if-let [params (realized-parameters method-entry)] 
                                     [:params (map (fn [i p] [:param {:index i}
                                                              [:value [:-cdata (pr-str p)]]])
                                                   (iterate inc 0) params)])])])]]]))))
