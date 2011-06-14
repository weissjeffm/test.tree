(ns test-clj.meta)
;;deals with test metadata structures



(defn get-1stlvl-tag [test tag]
  (-> (meta test) :test tag))



(defn configuration [test]
  (get-1stlvl-tag test :configuration))

(defn configuration? [test]
  (not (nil? (configuration test))))

(defn parameters [test]
					;not implemented yet
  )

(defn test-name [test]
  (format "%s/%s"
	  (str (:ns (meta test)))
	  (:name (meta test))))

(defn in-group? [group mytest]
  (some #{group}
   (mytest :groups)))

(defn in-groups? [groups myfn]
  (some #(in-group? % myfn) groups))




