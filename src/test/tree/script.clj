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

(defmacro deftest [testname & options+steps]
  (if (= testname :data-driven)
    (conj options+steps `deftest-datadriven)
    (let [[options steps] (split-opts options+steps)
          [steps dependent-tests] (split-deftests steps)]
      (merge `{:name ~testname
               :steps (serializable.fn/fn [] ~@steps)}
             (apply hash-map options)
             (if (not (empty? dependent-tests))
               {:more (vec dependent-tests)} {})))))

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
               group)))

(defn normalize "make sure it's either a single map or vector of maps - NOT a plain list"
  [tests]
  (if (map? tests) tests (vec tests)))

(defmacro defgroup [sym & opts+forms]
  (let [[opts forms] (split-opts opts+forms)
        opts (apply hash-map opts)
        groupname (str sym)]
    `(def ^:dynamic ~sym
       (-> ~(vec forms)
          normalize
          (add-group-setup ~groupname ~(:group-setup opts))
          (add-test-setup ~(:test-setup opts))
          (add-test-teardown ~(:test-teardown opts))
          (add-group-teardown ~groupname ~(:group-teardown opts))  ;;do last because we don't want to
                                                                   ;; alter tests after we've set up a wait for them.
          ))))

;;getting NPE trying to find the result for tests the group-teardown
;;is waiting for.  i assume they've been altered between the time we
;;set up the :blockers and when the tests actually run, so that we
;; don't find them in the reports later?