(defproject test.tree "1.0.0-SNAPSHOT"
  :description "A testNG like test harness for clojure"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [hickory "0.4.1"]
                 ;; [org.clojure/data.xml "0.0.7"] ;; waiting for bugfix DXML-17
                 [weissjeffm/data.xml "0.0.8-SNAPSHOT"]
                 [org.clojars.weissjeffm/serializable-fn "1.2.1-SNAPSHOT"]
                 [clj-stacktrace "0.2.6"]
                 [slingshot "0.10.3"]]
  
  :profiles {:dev {:jvm-opts ["-XX:-OmitStackTraceInFastThrow"]
                   :dependencies [[fn.trace "1.4.0-SNAPSHOT"]]}})
