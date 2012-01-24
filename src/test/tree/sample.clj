(ns test.tree.sample
  (:require (test.tree [builder :as builder]
                       [watcher :as watcher]
                       [reporter :as reporter])))

(def myvar "maindef")

(def sample (with-meta {:name "login"
                        :steps (fn [] (Thread/sleep 2000) (println "logged in"))
                        :more [{:name "create a widget"
                                :steps (fn [] (Thread/sleep 3000) (println "widget created") (throw (Exception. "woops"))) }
                               {:name "create a sprocket"
                                :steps (fn [] (Thread/sleep 5000) (println (str "sprocket created " myvar )))
                                :more [{:name "send a sprocket via email"
                                        :steps (fn [] (Thread/sleep 4000) (println "sent sprocket"))}]}
                               {:name "create a frob"
                                :steps (fn [] (Thread/sleep 4000) (println "frob created"))
                                :more [{:name "rename a frob"
                                        :steps (fn [] (Thread/sleep 4000) (println "frob renamed"))
                                        :more (builder/data-driven {:name "indivis by 5"
                                                                    :steps
                                                                    (fn [n] (-> n
                                                                              (mod 5)
                                                                              (= 0)
                                                                              (when (throw (Exception. "Divisible by 5! Oh noes!")))))} [[1] [20]  (with-meta (fn [] [7]) {:blockers (fn [_] [:blocker1])})])}
                                       {:name "delete a frob"
                                        :steps (fn [] (Thread/sleep 4000)
                                                 (throw (Exception. "woops, frob could not be deleted."))
                                                 (println "frob deleted"))
                                        :more [{:name "undelete a frob"
                                                :steps (fn [] (Thread/sleep 2000 (println "frob undeleted.")))}]}
                                       {:name "make sure 2 frobs can't have the same name"
                                        :steps (fn [] (Thread/sleep 4000) (println "2nd frob rejected"))}

                                       {:name "do that4"
                                        :steps (fn [] (Thread/sleep 4000) (println (str "there2.4 " myvar)))}
                                       {:name "do that5"
                                        :blockers (builder/filter-tests (every-pred (builder/named? ["delete a frob"])
                                                                          (complement reporter/passed?)))
                                        :steps (fn [] (Thread/sleep 4000) (println "there2.5"))}
                                       {:name "do that6"
                                        :blockers (builder/filter-tests (every-pred (builder/named?  ["final"]) (complement reporter/passed?)))
                                        :steps (fn [] (Thread/sleep 4000) (println (str "there2.6 " myvar)))}
                                       {:name "do that7"
                                        :blockers (builder/filter-tests (every-pred (builder/named? ["do that2"]) (complement reporter/passed?)))
                                        :steps (fn [] (Thread/sleep 4000) (println "there2.7"))
                                        :more (builder/data-driven {:name "do datadriven"}
                                                                   (fn [i]
                                                                     (Thread/sleep 1000)
                                                                     (println "did datadriven " i))
                                                                   [[1] [5] [22] ["hi"] [["a" "b"]]])}]}
                               
                               {:name "borg4"
                                :steps (fn [] (Thread/sleep 5000) (println "there4"))
                                :more [{:name "final"
                                        :steps (fn [] (Thread/sleep 4000) (println "there4.1"))}]}]}
              {:threads 4
               :watchers {
                          ;; :logs log-watcher
                          :onfail (watcher/on-fail
                                    (fn [t r] (println (format "Test %s failed!" (:name t)))))}}
               ;:thread-runner (fn [c] (throw (Exception. "waah")))
               ))
