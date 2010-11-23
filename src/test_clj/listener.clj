(ns test-clj.listener
  (:require [test-clj.meta :as meta])
  (:import [[org.testng.internal IResultListener]]))

(def events
     #{:onTestStart :onTestFinish :onTestFailed :onConfigurationStart :onConfigurationFailed
       :onTestSkipped :onConfigurationFinish})

(defn- log-fn [action-txt]
  (fn [test] (println (format "%s %s %s" action-txt (meta/test-name test)
			      (or (:))))))


(comment (def log-listener
      {:onTestStart (log-fn "Starting test")
       :onTestFinish (log-fn "Finishing test")
       :onTestFailed (log-fn "Test Failed:")
       :onConfigurationStart (log-fn "Starting configuration")
       :onConfigurationFailed (log-fn "Configuration Failed:")
       :onTestSkip (log-fn "Test Skipped:")
       :onConfigurationFinish (log-fn "Finishing configuration")
       })

	 (defprotocol test-listener "abstraction of a test listener"
	   (test-start [t] "called before test starts")
	   (test-finish [t] "called after test finishes")
	   (test-fail [t] "called after a test fails")
	   (configuration-start [t] "called before a configuration starts")
	   (configuration-finish [t] "called after a configuration finishes")
	   (test-skip [t] "called after a test is skipped")
	   (configuration-finish [t] "called after a configuration finishes"))

	 (deftype log-listener [] test-listener )

	 (extend ))

