(defproject test.tree "0.3.0-SNAPSHOT"
	  :description "A testNG like test harness for clojure"
	  :dependencies [[org.clojure/clojure "1.3.0"]
                         [org.clojure/clojure-contrib "1.2.0"]
                         [pretzel "0.2.2"]
                         [clj-stacktrace "0.2.3"]]
          :jvm-opts ["-Xmx256m"])
