(ns test-clj.sample-tests
  (:require [clojure.contrib.logging :as log]
            [test-clj.core :as test]))
					;sample tests

                                        ;------------------------------


(defn login-tests []
  {:name "Login sunny day"
   :tags #{:regression}
   :setup (test/fn [] (log/debug "start browser"))
   :procedure (test/fn []
                       (log/info "log in admin")
                       (log/info "verify ui is up"))
   :further-testing [{:name "Login bad user"
                      :procedure (test/fn []
                                          (log/info "log in asdf")
                                          (log/info "verify"))}
    
                     ]})

(defn provider-tests []
  {:name "provider create sunny day"
       :tags #{:regression}
       :setup (test/fn [] (log/debug "ensure admin"))
       :procedure (test/fn []
                           (log/info "create prov")
                           (log/info "verify create provider"))
       :further-testing [{:name "delete provider"
                          :procedure (test/fn []
                                              (log/info "delete")
                                              (log/info "verify"))}
                         
                         {:name "edit name of provider"
                          :procedure (test/fn []
                                              (log/info "edit name")
                                              (log/info "verify"))}
                         {:name "create a product"
                          :tags #{:products}
                          :procedure (test/fn []
                                              (log/info "create prod")
                                              (log/info "verify"))}
                         
                         ]})

(defn all-tests [] {:name "suite"
                    :further-testing [(login-tests) (provider-tests)]})



(defn all-login-tests []
  (->
   [{:name "config1"
     :configuration true
     :groups #{:blah}
     :procedure (test/fn []
                  (log/info "running configuration1")
                  (log/info "configuration1 complete."))}

    {:name "test1"
     :groups  #{:group1 :group2}
     :procedure (test/fn []
                         (log/info (str "Found config item" "blah")))}
    {:name "test2"
     :groups  #{:group2}
     :depends-on {:tests #{:test1}}
     :procedure (test/fn []
                         (log/info (str "Found config item" "blah")))}
    {:name "test3"
     :groups  #{:group3}
     :procedure (test/fn []
                         (log/info (str "Found config item" "blah")))}
    {:name "test4"
     :groups  #{:group3}
     :depends-on {:groups #{:group2}}
     :procedure (test/fn []
                         (log/info (str "Found config item" "blah")))}
    {:name "test5"

     :groups #{:group4 :group5}
     :depends-on {:tests #{"test2" "test3"} :groups #{:group4}}
     :procedure (test/fn []
                  (log/info "running test2")
                  (log/info (str "Found config item" "blah"))
                  (log/info "test2 complete"))}

    {:name "test6"
     :description "tests the widget by greeping the zorp."
     :groups #{:group5}
     :depends-on {:tests #{"test1"} :groups #{:group3}}
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

