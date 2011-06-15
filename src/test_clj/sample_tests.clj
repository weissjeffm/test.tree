(ns test-clj.sample-tests
  (:refer-clojure :exclude [fn])
  (:require [clojure.contrib.logging :as log]
            [test-clj.core :as test])
  (:use [serializable.fn :only [fn]]))
					;sample tests
					;------------------------------

(def login-before-test {:name "logout"
                        :configuration true
                        :groups #{:login}
                        :procedure (fn []
                                     (log/info "logging out")
                                     (log/info "logout complete."))})

(def login-after-test {:name "verify-user"
                       :configuration true
                       :groups #{:login}
                       :procedure (fn []
                                    (log/info "verifying user")
                                    (log/info "verify complete."))})
(def login-tests [{:name "config1"
                   :configuration true
                   :groups #{:blah}
                   :procedure (fn []
                                (log/info "running configuration1")
                                (log/info "configuration1 complete."))}

                  {:name "test2"
                   :groups  #{:group2 :group3}
                   :procedure (fn []
                                (log/info "running test2")
                                (log/info (str "Found config item" "blah"))
                                (log/info "test2 complete"))}
                  {:name "test1"
                   :description "tests the widget by flurbing the blob."
                   :groups #{:group1 :group2}
                   :depends-on {:tests #{"test2"} :groups #{:group4}}
                   :procedure (fn []
                                (log/info "running test2")
                                (log/info (str "Found config item" "blah"))
                                (log/info "test2 complete"))}]
  )

(defn all-login-tests []
  (test/before-tests login-before-test login-tests))

(comment (defn ^{:test {:configuration :beforeSuite
                :groups #{:group1 :group2}}} 
   config1 [_]
   (do (log/info "running configuration1")
       (log/info "configuration1 complete.")))

 (defn ^{:test {:groups #{:group2 :group3}}} 
   test2 [_] 

   (do (log/info "running test2")
       (log/info (str "Found config item" (:build @config-data)))
       (log/info "test2 complete")))

 (defn ^{:test {:groups #{:group1 :group2} 
                :dependsOnTests #'test2}}
   test1 [_] 
   (do (log/info "running test1") 
       (log/info "test1 complete")))

 (defn ^{:test {:groups #{:group2 :group3} 
                :dependsOnTests #'test2}} 
   test3 [_] 
   (do (log/info "running test3") 
       (throw (RuntimeException. "test failed!")) 
       (log/info "test3 complete")))

 (defn ^{:test {:groups #{:group1 :group3} 
                :dependsOnTests #'test3}} 
   test4 [_] 
   (do (log/info "running test4") 
       ;;(throw (RuntimeException. "test failed!")) 
       (log/info "test4 complete")))

 (defn ^{:test {:groups #{:group1 :group2}
		:configuration :beforeTest}}
   config5 [_] 
   (do (log/info "running configuration5") 
       (log/info "configuration5 complete")))

 (defn ^{:test {:groups #{:group1 :group2}
		:configuration :afterTest}}
   config6 [_] 
   (do (log/info "running configuration6") 
       (log/info "configuration6 complete"))))

					;end sample tests
					;--------------------------------

