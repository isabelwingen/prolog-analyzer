(ns prolog-analyzer.domain)



(defn is-subdom? [dom1 dom2] false)


(defn calculate-knf [arglist]
  (let [[ands ors] (vals (group-by #(= :or (:spec %)) arglist))]
    (reduce #(for [a %1
                   b (:arglist %2)]
               {:spec :and :arglist (conj (:arglist a) b)})
            [{:spec :and :arglist ands}]
            ors)))

(defmulti to-knf :spec)

(defmethod to-knf :and [dom]
  (-> dom
      (update :arglist (partial map to-knf))
      (update :arglist (partial map #(if (= :and (:spec %)) (get % :arglist %) %)))
      (update :arglist flatten)
      (update :arglist calculate-knf)
      (assoc :spec :or)
      ))

(defmethod to-knf :or [dom]
  (-> dom
      (update :arglist (partial map to-knf)) ;; only :or and simple specs
      (update :arglist (partial map #(if (= :or (:spec %)) (get % :arglist %) %))) 
      (update :arglist flatten)
      ))

(defmethod to-knf :default [dom]
  dom)

(defn merge [dom1 dom2]
  (to-knf {:spec :and :arglist (list dom1 dom2)}))


(defn union [dom1 dom2]
  (cond
    (is-subdom? dom1 dom2) dom2
    (is-subdom? dom2 dom1) dom1
    :else (to-knf {:spec :or :arglist (list dom1 dom2)})))


(defn intersect [dom1 dom2]
  (cond
    (is-subdom? dom1 dom2) dom1
    (is-subdom? dom2 dom1) dom2
    :else (to-knf {:spec :and :arglist (list dom1 dom2)})))
