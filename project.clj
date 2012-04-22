(defproject test.tree "0.6.1-SNAPSHOT"
  :description "A testNG like test harness for clojure"
  :aot [test.tree.builder]
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/core.incubator "0.1.0"]
                 [weissjeffm/clojure.prxml "1.3.0-SNAPSHOT"]
                 [org.clojars.weissjeffm/serializable-fn "1.2.1-SNAPSHOT"]
                 [clj-stacktrace "0.2.4"]
                 [slingshot "0.10.1"]])
