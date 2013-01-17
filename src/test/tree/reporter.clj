(ns test.tree.reporter
  (:require [clojure.prxml :as xml]
            [clojure.set :as sets]
            test.tree.zip)
  (:refer-clojure :exclude [key val])
  (:use clojure.pprint 
        [clj-stacktrace.repl :only [pst-str]]))



(defmulti exception :wrapper)
(defmethod exception nil [e] (:object e))
(defmethod exception :default [e] (:wrapper e))


;;report accessor fn's
(def result ::result)
(def blocked-by ::blocked-by)
(def outcome ::outcome)
(def error ::error)
(def thread ::thread)
(def parameters ::parameters)
(def start-time ::start-time)
(def end-time ::end-time)
(def pass ::pass)
(def fail ::fail)
(def skip ::skip)
(def status ::status)

;; Functions to process testentries (mapentry of test to report)
(def key first)
(def val second) ;; works on lists or mapentries

(defn entry [m k]
  (list k (get m k)))

(def entry-test-result (comp result val))

(def entry-blocked-by (comp blocked-by entry-test-result))

(def entry-outcome (comp outcome entry-test-result))

(def entry-error (comp error entry-test-result))

(def entry-thread (comp thread entry-test-result))

(def entry-realized-parameters (comp parameters entry-test-result))

(def entry-configuration? (comp boolean :configuration key))

(def entry-testgroup (comp :groups key))

(def entry-start-time (comp start-time entry-test-result))

(def entry-end-time (comp end-time entry-test-result))

(defn entry-passed? [testentry]
  (= (entry-outcome testentry) pass))

(defn entry-skipped? [testentry]
  (= (entry-outcome testentry) skip))

(defn entry-failed? [testentry]
  (= (entry-outcome testentry) fail))

(defn entry-execution-time [testentry]
  (let [[start end] (juxt [entry-start-time entry-end-time] testentry)]
    (if (and start end)
      (/ (- end start) 1000.0)
      0)))

(def entry-status (comp status val))


;; Other functions

(defn init-reports [z]
  (zipmap (test.tree.zip/nodes z)
          (repeat {::status :waiting})))

(defn test-passed?
  "Given a reference to reports, wait for test to complete (if not
  already) and return whether it passed."
  [report-ref test]
  (let [test-result #(-> report-ref deref (get test) result outcome)]
    (loop [r (test-result)]
      (if r
        (= r pass)
        (recur (do (Thread/sleep 100)
                   (test-result)))))) ;;there's probably a better way to do this than polling
  )

(defn total-time [report]
  (reduce + (map entry-execution-time report)))

(defn blocker-report [report]
  (->> report
     vals
     (mapcat #(-> % result deref blocked-by))
     (filter (complement nil?))
     frequencies))

(defn- format-exception-msg [t]
  (format "On thread %s: %s"
          (entry-thread t)
          (-> t entry-error :message))) 

;; Functions for generating specific format of report

(defn junit-report "Produce an xml report consistent with the
                    junit report schema.  Tries to be especially
                    compatible with Jenkins and ReportNG."
  [report]
  (let [by-result (group-by entry-outcome report)
        fails (by-result fail)
        skips (by-result skip)
        passes (by-result pass)
        [numfail numskip numpass] (map count [fails skips passes])
        total (+ numfail numskip numpass)
        info (fn [[t :as e]]
               {:name (let [p (:parameters t)]
                        (if p (pr-str p) (:name t)))
                :time (entry-execution-time e)
                :classname (:name t)})]
    (binding [xml/*prxml-indent* 2]
      (xml/prxml [:decl! {:version "1.0"} ]
                 [:testsuite {:tests (str total)
                              :failures (str numfail)
                              :errors "0"
                              :skipped (str numskip)
                              :time (str (total-time report))}
                  (concat (for [fail fails]
                            (do (println fail) [:testcase (info fail)
                                    (let [err (entry-error fail)
                                          obj (:object err)]
                                      [:failure {:type (or (:type obj)
                                                           (-> obj .getClass .getName))
                                                 :time (entry-execution-time fail)
                                                 :message (format-exception-msg fail)}
                                       [:cdata! (-> fail entry-error exception pst-str)]])]))
                          (for [skip skips]
                            (let [reason (entry-blocked-by skip)]
                              [:testcase (info skip)
                               [:skipped (if reason
                                           {:message (format "On thread %s: %s"
                                                             (entry-thread skip)
                                                             (str reason))}
                                           {})]]))
                          (for [pass passes]
                            [:testcase (info pass)]))]))))

(def testng-dateformat (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss'Z'"))

(defn syntax-highlighter [sh-url]
  (fn [s]
    (let [url #(str sh-url %)]
      (format "<script type=\"text/javascript\" src=\"%s\"></script>
               <script type=\"text/javascript\" src=\"%s\"></script>
               <link href=\"%s\" rel=\"stylesheet\"/>
               <link href=\"%s\" rel=\"stylesheet\"/>
               <script type=\"text/javascript\">SyntaxHighlighter.all()</script>
               <pre class=\"brush: clj;gutter: false; toolbar: false\">%s</pre> "
              (url "scripts/shCore.js")
              (url "scripts/shBrushClojure.js")
              (url "styles/shCore.css")
              (url "styles/shCoreEmacs.css")
              s))))

;;a rebindable function to do syntax highlighting on some code/data in
;;a report.  Should take text and return the syntaxhighlighted html
(def ^:dynamic syntax-highlight identity)


(defn testng-report "Produce an xml report consistent with the
                    testng report schema.  Tries to be especially
                    compatible with Jenkins."
  [report]
  (let [by-result (group-by entry-outcome report)
        fails (by-result fail)
        skips (by-result skip)
        passes (by-result pass)
        [numfail numskip numpass] (map count [fails skips passes])
        total (+ numfail numskip numpass)
        by-class (group-by entry-testgroup report)
        to-ms-str #(-> (* % 1000) Math/round str)
        suite-duration-ms (to-ms-str (total-time report))
        date-format (fn [unixdate] (.format testng-dateformat (java.util.Date. unixdate)))
        info (fn [[t tr :as e]] 
               (merge {:name (:name t)
                       :duration-ms (to-ms-str (entry-execution-time e))
                       ::status (cond (entry-skipped? e) "SKIP"
                                     (entry-passed? e) "PASS"
                                     (entry-failed? e) "FAIL")
                       :signature (try (format "%s%s" (:name t) (-> t :steps second))
                                       (catch Exception _ "sig"))
                       :started-at (date-format (-> tr result :start-time))
                       :finished-at (date-format (-> tr result :end-time))
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
                    (for [[groups method-entries] by-class]
                      [:class {:name (apply str (interpose "."
                                                           (or
                                                            (-> groups reverse vec (conj (first groups)))
                                                            ["rootClass"])))}
                       (for [method-entry method-entries]
                         [:test-method (info method-entry)
                          (when (entry-skipped? method-entry)
                            [:exception {:class "Skipped"}
                             [:message [:cdata! (format "Blocked by: %s"
                                                        (-> method-entry val blocked-by pr-str))]]
                             [:short-stacktrace [:cdata! "Skips are not errors, no stacktrace."]]])
                          (when (entry-failed? method-entry)
                            (let [err (-> method-entry entry-error (dissoc :stack-trace)) 
                                  e (exception err)
                                  msg (format-exception-msg method-entry)
                                  pretty-st (pst-str e)
                                  not-empty (fn [s] (and s (-> s .length (> 0))))]
                              (when (every? not-empty [msg pretty-st])
                                [:exception {:class (-> e .getClass str)}
                                 [:message [:cdata!
                                            (->> err pprint with-out-str syntax-highlight)]]
                                 [:short-stacktrace [:cdata! pretty-st]]])))
                          (if-let [params (entry-realized-parameters method-entry)] 
                            [:params (map (fn [i p] [:param {:index i}
                                                    [:value [:cdata! (pr-str p)]]])
                                          (iterate inc 0) params)])])])]]]))))
