(ns test-clj.sample-tests
  (:require [clojure.contrib.logging :as log]
            [test-clj.core :as test]))
					;sample tests

                                        ;------------------------------

;; :depends should be a fn of one argument (the zipper tree) that returns all the failed
;; deps, or nil if they all passed.  

(def login (test/fn [user pw]
               (log/info (format "Logging in as %s and pw %s." user pw))))

(defn login-tests []
  {:name "Login sunny day"
   :tags #{:regression}
   :procedure (test/fn []
                       (login "admin" "admin")
                       (log/info "verify ui is up"))
   :further-testing (test/data-driven {:name "Login bad user"}
                                      login [["%%%" ""]
                                             ["" ""]
                                             ["admin  " ""]
                                             [" " " "]])})

(defn provider-tests []
  {:name "provider create sunny day"
   :tags #{:regression}
   :procedure (test/fn []
                       (log/info "create prov")
                       (log/info "verify create provider")
                       (throw (Exception. "create provider failed")))
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

(defn environment-tests []
  {:name "environment create sunny"
   :procedure (test/fn []
                       (log/info "creating env")
                       (throw (Exception. "could not create env")))
   :further-testing [{:name "delete env"
                      :procedure (test/fn []
                                          (log/info "deleting"))}]})


(defn promotion-tests []
  {:name "simple promote product"
   :pre-fn (test/unsatisfied (test/by-name ["environment create sunny"]))
   :procedure (test/fn []
                       (log/info "promote prod"))
   :further-testing [{:name "change set cleared"
                      :procedure (test/fn []
                                          (log/info "promoting")
                                          (log/info "change set empty"))}]})

(defn all-tests [] {:name "startup"
                    :procedure (test/fn [] nil)
                    :further-testing [(login-tests) (provider-tests) (environment-tests) (promotion-tests)]})



(comment (defn all-login-tests []
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
                                            (log/info "verify complete."))}]))))



					;end sample tests
					;--------------------------------

