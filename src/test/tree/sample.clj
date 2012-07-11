(ns test.tree.sample
  (:use slingshot.slingshot
        test.tree.script)
  (:require (test.tree [builder :as builder]
                       [watcher :as watcher]
                       [reporter :as reporter])))

(def myvar "maindef")

(defgroup sprockets
  :group-setup '(println "setting up sprockets!")
  :group-teardown '(println "tearing down sprockets!")

  (deftest "create a sprocket"
    (do (println (str "sprocket created " myvar )))
    
    (deftest "send a sprocket via email"
      (do  (println "sent sprocket")))))

(def sample2 (with-meta (deftest "login"
                          (do (println "logged in"))

                          (deftest "create a widget"
                            (do (println "widget created")
                                (throw (Exception. "woops"))))
                          
                          sprockets

                          (deftest "create a frob"
                            (do  (println "frob created"))

                            (deftest "rename a frob"
                              (do  (println "frob renamed"))

                              (defddtest "indivis by 5"
                                (-> n (mod 5)
                                   (= 0)
                                   (when (throw+ {:type :divisibility :msg "Divisible by 5! Oh noes!"})))

                                [n]
                                [[1]
                                 [20]
                                 (with-meta [7] {:blockers (fn [_] [:blocker1])})]))
                            (deftest "delete a frob"
                              (do (throw (Exception. "woops, frob could not be deleted."))
                                  (println "frob deleted"))

                              (deftest "undelete a frob"
                                (do  (println "frob undeleted."))))

                            (deftest "make sure 2 frobs can't have the same name"
                              (do  (println "2nd frob rejected")))

                            (deftest "do that4"
                              (let [x "hi!"] (println (str "there2.4 " myvar x))))

                            (deftest "do that5"
                              :blockers (builder/filter-tests (every-pred (builder/named? ["delete a frob"])
                                                                          (complement reporter/passed?)))
                              (do  (println "there2.5"))

                              (deftest "do that6"
                                :blockers (builder/filter-tests (every-pred (builder/named?  ["final"]) (complement reporter/passed?)))
                                (do  (println (str "there2.6 " myvar)))))

                            (deftest "do that7"
                              :blockers (builder/filter-tests (every-pred (builder/named? ["do that2"]) (complement reporter/passed?)))
                              (do  (println "there2.7"))   

                              (defddtest  "do datadriven"
                                (do
                                  (println "did datadriven " i)
                                  (println "time is " (System/currentTimeMillis)))
                                [i]
                                [[1] [5] [22] ["hi"] [["a" "b"]] ['(System/currentTimeMillis)]])))
                         
                          (deftest "borg4"
                            (do  (println "there4"))

                            (deftest "final"
                              (do  (println "there4.1")))))
               {:threads 4
                :watchers {:stdout watcher/stdout-log-watcher
                           ;; :logs log-watcher
                           :onfail (watcher/on-fail
                                    (fn [t r] (println (format "Test %s failed!" (:name t)))))}}
                                        ;:thread-runner (fn [c] (throw (Exception. "waah")))
               ))



(comment
  (def blork [1 2 3])

  (defddtest "my dd test"
    :blockers (constantly [])
    
    [s i]
    (= (count s) i)

    (for [x blork] [(str x) (inc x)]))


  (defddtest "test name"
    (= (count s ) i)

    [s i]
    (for [x blork] [(str x) (inc x)]))

  (defdtest "name"
    options
    argsvec
    steps-expr
    data-expr)
  )