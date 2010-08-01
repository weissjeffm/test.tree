(ns test-clj.listener
  (:require [test-clj.meta :as meta]))

(def events
     #{:onTestStart :onTestFinish :onTestFailed :onConfigurationStart :onConfigurationFailed
       :onTestSkipped :onConfigurationFinish})

(defn- log-fn [action-txt]
  (fn [test] (println (format "%s %s %s" action-txt (meta/test-name test)
			      (or (:))))))


(def log-listener
     {:onTestStart (log-fn "Starting test")
      :onTestFinish (log-fn "Finishing test")
      :onTestFailed (log-fn "Test Failed:")
      :onConfigurationStart (log-fn "Starting configuration")
      :onConfigurationFailed (log-fn "Configuration Failed:")
      :onTestSkip (log-fn "Test Skipped:")
      :onConfigurationFinish (log-fn "Finishing configuration")
      })