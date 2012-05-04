(ns test.tree.script
  (:use [test.tree.builder :only [data-driven before-all after-all before-each after-each]]))

(defn split-opts [args]
  "split a list at the first item that's in an even index (zero based)
   that is not a keyword"
  (->> args
     (map-indexed vector)
     (split-with (fn [i] (or (odd? (first i))
                            (keyword? (second i)))))
     (map #(map second %))))

(defn split-deftests [args]
  (let [deftest-var (resolve 'deftest)]
    (split-with #(not= (resolve (first %)) deftest-var) args)))

(defn normalize "make sure it's either a single map or vector of maps - NOT a plain list"
  [tests]
  (if (map? tests) tests
      (vec tests)))

(defmacro deftest
  "Defines a test with name testname (a string), optional pairs of
  keywords and their values, and forms that comprise the test
  procedure. Finally, more nested deftest forms can optionally be
  added at the end. Expands into a plain map. See the main test.tree
  documentation for the available options and how nesting should be
  done. Example: (deftest 'add 2 and 2' :description 'adds
  numbers' (assert (-> (+ 2 2) (= 4)))) "
  [testname & options+steps]
  (let [[options allsteps] (split-opts options+steps)
        optmap (apply hash-map options)
        [steps dependent-tests] (split-deftests allsteps)]
    (if (:data-driven optmap)
      (conj (concat (mapcat vec (dissoc optmap :data-driven))
                    allsteps) testname `deftest-datadriven)
      (merge `{:name ~testname
               :steps (serializable.fn/fn [] ~@steps)}
             optmap
             (if (not (empty? dependent-tests))
               {:more `(vec (flatten ~(vec dependent-tests)))} {})))))

(defmacro deftest-datadriven [testname & options+kw+data]
  (let [[options [kw & rows]] (split-opts options+kw+data)
        thistest (merge {:name testname
                         :steps kw}
                        (apply hash-map options))]
    `(data-driven ~thistest ~@rows)))

(defn add-test-setup [group setup]
  (if setup
    (before-each setup group)
    group))

(defn add-test-teardown [group teardown]
  (if teardown
    (after-each teardown group)
    group))

(defn add-group-setup [group groupname setup]
  (if (and (map? group) (not setup))
    group
    (before-all {:name (format "Setup for %s" groupname)
                 :configuration true
                 :steps (or setup (constantly nil))}
                group)))

(defn add-group-teardown [group groupname teardown]
  (if teardown
    (after-all {:name (format "Teardown for %s" groupname)
                :configuration true
                :steps teardown}
               group)
    group))

(defmacro defgroup
  "Defines a group of tests as a normal var, named sym. Inside the
   form should be a optional pairs of keywords and their values,
   followed by one or more tests. Tests are most easily written using
   deftest, but any thing that returns valid test maps are allowed.
   The options are
    :group-setup (a function to run before anything in the
    group)
    :group-teardown (function to run after everything in the group)
    :test-setup (function to run before each and every test in the group)
    :test-teardown (function to run after each and every test in the
    group)
   All the functions should take no arguments. defgroup forms
    cannot be nested, but you can refer to a group anywhere using its
    var (including in another defgroup). See also deftest." [sym &
    opts+forms]
  (let [[opts forms] (split-opts opts+forms)
        opts (apply hash-map opts)
        groupname (str sym)]
    `(def ^:dynamic ~sym
       (-> ~(vec forms)
          normalize
          (add-group-setup ~groupname ~(:group-setup opts))
          (add-test-setup ~(:test-setup opts))
          (add-test-teardown ~(:test-teardown opts))
          (add-group-teardown ~groupname ~(:group-teardown opts)) ;;do last because we don't want to
          ;; alter tests after we've set up a wait for them.
          ))))

(comment (def mytest
           (defgroup xyz
             :test-setup (fn [] (println "pre-test thing"))
             :group-setup (fn [] (println "configuring the whole group"))
             :group-teardown (fn [] (println "cleaning up group!"))
             :test-teardown (fn [] (println "cleanin up test"))
           
             (deftest "asdf"
               (+ 1 1))

             (deftest "zyz"
               (- 2 1))

             (deftest "bar"
               (/ 5 2)))))