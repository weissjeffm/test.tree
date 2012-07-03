(ns test.tree.sample
  (:use slingshot.slingshot
        test.tree.script)
  (:require (test.tree [builder :as builder]
                       [watcher :as watcher]
                       [reporter :as reporter])))

(def myvar "maindef")

(comment (def sample (with-meta {:name "login"
                         :steps (fn [] (Thread/sleep 200) (println "logged in"))
                         :more [{:name "create a widget"
                                 :steps (fn [] (Thread/sleep 300) (println "widget created") (throw (Exception. "woops"))) }
                                {:name "create a sprocket"
                                 :steps (fn [] (Thread/sleep 500) (println (str "sprocket created " myvar )))
                                 :more [{:name "send a sprocket via email"
                                         :steps (fn [] (Thread/sleep 400) (println "sent sprocket"))}]}
                                {:name "create a frob"
                                 :steps (fn [] (Thread/sleep 400) (println "frob created"))
                                 :more [{:name "rename a frob"
                                         :steps (fn [] (Thread/sleep 400) (println "frob renamed"))
                                         :more (builder/data-driven {:name "indivis by 5"
                                                                     :steps
                                                                     `(-> n
                                                                         (mod 5)
                                                                         (= 0)
                                                                         (when (throw+ {:type :divisibility :msg "Divisible by 5! Oh noes!"})))}
                                                                    `[[n] [1] [20]  (with-meta [7] {:blockers (fn [_] [:blocker1])})])}
                                        {:name "delete a frob"
                                         :steps (fn [] (Thread/sleep 400)
                                                  (throw (Exception. "woops, frob could not be deleted."))
                                                  (println "frob deleted"))
                                         :more [{:name "undelete a frob"
                                                 :steps (fn [] (Thread/sleep 200 (println "frob undeleted.")))}]}
                                        {:name "make sure 2 frobs can't have the same name"
                                         :steps (fn [] (Thread/sleep 400) (println "2nd frob rejected"))}

                                        {:name "do that4"
                                         :steps (fn [] (Thread/sleep 400) (println (str "there2.4 " myvar)))}
                                        {:name "do that5"
                                         :blockers (builder/filter-tests (every-pred (builder/named? ["delete a frob"])
                                                                                     (complement reporter/passed?)))
                                         :steps (fn [] (Thread/sleep 400) (println "there2.5"))}
                                        {:name "do that6"
                                         :blockers (builder/filter-tests (every-pred (builder/named?  ["final"]) (complement reporter/passed?)))
                                         :steps (fn [] (Thread/sleep 400) (println (str "there2.6 " myvar)))}
                                        {:name "do that7"
                                         :blockers (builder/filter-tests (every-pred (builder/named? ["do that2"]) (complement reporter/passed?)))
                                         :steps (fn [] (Thread/sleep 400) (println "there2.7"))
                                         :more (builder/data-driven {:name "do datadriven"
                                                                     :steps
                                                                     (fn [i]
                                                                       (Thread/sleep 1000)
                                                                       (println "did datadriven " i))}
                                                                    [[1] [5] [22] ["hi"] [["a" "b"]]])}]}
                               
                                {:name "borg4"
                                 :steps (fn [] (Thread/sleep 500) (println "there4"))
                                 :more [{:name "final"
                                         :steps (fn [] (Thread/sleep 400) (println "there4.1"))}]}]}
               {:threads 4
                :watchers {:stdout watcher/stdout-log-watcher
                           ;; :logs log-watcher
                           :onfail (watcher/on-fail
                                    (fn [t r] (println (format "Test %s failed!" (:name t)))))}}
                                        ;:thread-runner (fn [c] (throw (Exception. "waah")))
               )))

(defgroup sprockets
  :group-setup `(println "setting up sprockets!")
  :group-teardown `(println "tearing down sprockets!")

  (deftest "create a sprocket"
    `(do (Thread/sleep 500)
         (println (str "sprocket created " myvar )))
    
    (deftest "send a sprocket via email"
      `(do (Thread/sleep 400) (println "sent sprocket")))))

(def sample2 (with-meta (deftest "login"
                          `(do (println "logged in"))

                          (deftest "create a widget"
                            `(do (println "widget created")
                                 (throw (Exception. "woops"))))
                          
                          sprockets

                          (deftest "create a frob"
                            `(do  (println "frob created"))

                            (deftest "rename a frob"
                              `(do  (println "frob renamed"))

                              (deftest "indivis by 5"
                                :data-driven `[[n] [1] [20]  ~(with-meta `[7] {:blockers (fn [_] [:blocker1])})]
                                `(-> n (mod 5)
                                    (= 0)
                                    (when (throw+ (:type :divisibility :msg "Divisible by 5! Oh noes!"))))))
                            (deftest "delete a frob"
                              `(do (throw (Exception. "woops, frob could not be deleted."))
                                   (println "frob deleted"))

                              (deftest "undelete a frob"
                                `(do  (println "frob undeleted."))))

                            (deftest "make sure 2 frobs can't have the same name"
                              `(do  (println "2nd frob rejected")))

                            (deftest "do that4"
                              `(do  (println (str "there2.4 " myvar))))

                            (deftest "do that5"
                              :blockers (builder/filter-tests (every-pred (builder/named? ["delete a frob"])
                                                                          (complement reporter/passed?)))
                              `(do  (println "there2.5"))

                              (deftest "do that6"
                                :blockers (builder/filter-tests (every-pred (builder/named?  ["final"]) (complement reporter/passed?)))
                                `(do  (println (str "there2.6 " myvar)))))

                            (deftest "do that7"
                              :blockers (builder/filter-tests (every-pred (builder/named? ["do that2"]) (complement reporter/passed?)))
                              `(do  (println "there2.7"))   

                              (deftest  "do datadriven"
                                :data-driven  `[[i] [1] [5] [22] ["hi"] [["a" "b"]]]
                                `(do
(println "did datadriven " i)))))
                         
                          (deftest "borg4"
                            `(do  (println "there4"))

                            (deftest "final"
                              `(do  (println "there4.1")))))
               {:threads 4
                :watchers {:stdout watcher/stdout-log-watcher
                           ;; :logs log-watcher
                           :onfail (watcher/on-fail
                                    (fn [t r] (println (format "Test %s failed!" (:name t)))))}}
                                        ;:thread-runner (fn [c] (throw (Exception. "waah")))
               ))
