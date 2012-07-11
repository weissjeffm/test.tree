(ns test.tree.script
  (:use [test.tree.builder :only [data-driven before-all after-all before-each after-each tmap]]))

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
      (vec (flatten tests))))

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
        [steps & dependent-tests]  allsteps
        basetest {:name testname
                  :steps `(quote ~steps)
                  :ns *ns*}]
    (merge basetest
           optmap
           (if-not (empty? dependent-tests)
             `{:more (vec (flatten ~(vec dependent-tests)))}
             `{}))))

(defmacro defddtest [testname & options+steps+data]
  (let [[options [steps params data]] (split-opts options+steps+data)
        thistest (merge `{:name ~testname
                         :steps (quote ~steps)
                         :ns *ns*}
                        (apply hash-map options))]
    `(data-driven ~thistest (quote ~params) (quote ~data))))

;;these next few will need to change now that we're using
;;quoted forms rather than functions

(defn add-test-setup [group setup]
  (if setup
    (before-each setup group)
    group))

(defn add-test-teardown [group teardown]
  (if teardown
    (after-each teardown group)
    group))

(def ns-from-sym (comp find-ns symbol namespace))

(defn add-group-setup [group groupsym setup blockers]
  (if (and (map? group) (not setup))
    group
    (before-all (merge {:name (format "Setup for %s" (name groupsym))
                        :configuration true
                        :ns (ns-from-sym groupsym)
                        :steps (or setup nil)}
                       (if blockers {:blockers blockers} {}))
                group)))

(defn add-group-teardown [group groupsym teardown]
  (if teardown
    (after-all {:name (format "Teardown for %s" (name groupsym))
                :configuration true
                :ns (ns-from-sym groupsym)
                :steps teardown}
               group)
    group))

(defn insert-group-name [tree groupsym]
  (let [groupname (name groupsym)]
    (tmap (fn [test]
            (update-in test [:groups]
                       (fn [grps] (if grps (conj grps groupname)
                                     [groupname]))))
          tree)))

(defn defgroup* [forms groupsym opts]
  (-> forms
     normalize
     (add-group-setup groupsym (:group-setup opts) (:blockers opts))
     (add-test-setup (:test-setup opts))
     (insert-group-name groupsym) ; need to do this before group
                                   ; teardown, otherwise test will
                                   ; gain extra key and won't match the
                                   ; result map keys
     
     (add-test-teardown (:test-teardown opts))
     (add-group-teardown groupsym (:group-teardown opts))))

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
    :blockers (function returning a list of blockers for this whole group
   of tests.  See main test.tree documentation.)
  All the functions should take no arguments unless
   otherwise specified. defgroup forms cannot be nested, but you can
   refer to a group anywhere using its var (including in another
   defgroup). See also deftest." [sym & opts+forms]
  (let [[opts forms] (split-opts opts+forms)
        opts (apply hash-map opts)]
    `(def ^:dynamic ~sym
       (defgroup* ~(vec forms) (quote ~(symbol (str *ns*) (str sym))) ~opts))))

(comment (defgroup abc
           (deftest "blargh"
             (+ 0 0)))
         
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
             (/ 5 2))

           abc))