(ns test.tree.sample
  (:use slingshot.slingshot)
  (:require (test.tree [builder :as builder]
                       [watcher :as watcher]
                       [reporter :as reporter])))

(def myvar "maindef")

(def sample {:name "login"
             :steps (fn [] (Thread/sleep 200) (println "logged in"))
             :more [{:name "create a widget"
                     :steps (fn [] (Thread/sleep 300) (println "widget created") (throw (Exception. "woops"))) }
                    {:name "create a sprocket"
                     :configuration true
                     :steps (fn [] (Thread/sleep 500) (println (str "sprocket created " myvar )))
                     :more [{:name "send a sprocket via email"
                             :steps (fn [] (Thread/sleep 400) (println "sent sprocket"))}]}
                    {:name "create a frob"
                     :steps (fn [] (Thread/sleep 400) (println "frob created"))
                     :more [{:name "rename a frob"
                             :steps (fn [] (Thread/sleep 400) (println "frob renamed"))
                             :more (builder/data-driven {:name "indivis by 5"
                                                         :steps
                                                         (fn [n] (-> n
                                                                   (mod 5)
                                                                   (= 0)
                                                                   (when (throw+ {:type :divisibility :msg "Divisible by 5! Oh noes!"}))))} [[1] [20]  (with-meta (fn [] [7]) {:blockers (list (fn [_] [:blocker1]))})])}
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
                             :blockers (list (builder/blocking-tests "delete a frob"))
                             :steps (fn [] (Thread/sleep 400) (println "there2.5"))}
                            {:name "do that6"
                             :blockers (list (builder/blocking-tests "final"))
                             :steps (fn [] (Thread/sleep 400) (println (str "there2.6 " myvar)))}
                            {:name "do that7"
                             :blockers (list (builder/blocking-tests "do that2"))
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
                                          ;:thread-runner (fn [c] (throw (Exception. "waah")))
  )

(def options {:threads 4
              :watchers {:stdout watcher/stdout-log-watcher
                         ;; :logs log-watcher
                         :onfail (watcher/on-fail
                                  (fn [t r] (println (format "Test %s failed!" (:name t)))))}})
