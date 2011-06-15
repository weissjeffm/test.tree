(ns test-clj.sample-tests
  (:require [clojure.contrib.logging :as log]
            [test-clj.core :as test]))
					;sample tests

                                        ;------------------------------

(comment )

(defn all-login-tests []
  (->
   [{:name "config1"
     :configuration true
     :groups #{:blah}
     :procedure (test/fn []
                  (log/info "running configuration1")
                  (log/info "configuration1 complete."))}

    {:name "test2"
     :groups  #{:group2 :group3}
     :procedure (test/fn []
                  (log/info "running test2")
                  (log/info (str "Found config item" "blah"))
                  (log/info "test2 complete"))}
    {:name "test1"

     :groups #{:group1 :group2}
     :depends-on {:tests #{"test2"} :groups #{:group4}}
     :procedure (test/fn []
                  (log/info "running test2")
                  (log/info (str "Found config item" "blah"))
                  (log/info "test2 complete"))}

    {:name "test3"
     :description "tests the widget by greeping the zorp."
     :groups #{:group1 :group2}
     :depends-on {:tests #{"test2"} :groups #{:group4}}
     :procedure (test/fn []
                  (log/info "running test2")
                  (throw (Exception. "Oh nooooo"))
                  (log/info "test2 complete"))}]

   (test/before-tests [{:name "logout"
                        :configuration true
                        :groups #{:login}
                        :procedure (test/fn []
                                     (log/info "logging out")
                                     (log/info "logout complete."))}])
   (test/after-tests [{:name "verify-user"
                        :configuration true
                        :groups #{:login}
                        :procedure (test/fn []
                                     (log/info "verifying user")
                                     (log/info "verify complete."))}])))



					;end sample tests
					;--------------------------------

