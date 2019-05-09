(ns prolog-analyzer.analyzer.type-analyzer
  (:require
   [clojure.string]))


(def x (read-string (str "[" (slurp "results/prologmud_I7_0a2d94d31.edn") "]")))

(defn non-atomic-keys [keys]
  (remove #(#{:atomic, :atom, :number, :string, :integer, :float :empty-list} (:record-type %)) keys))


(defn contains-var? [m]
  (case (:record-type m)
    :var true
    :list (or (contains-var? (:head m)) (contains-var? (:tail m)))
    :compound (some contains-var? (:arglist m))
    :empty-list false
    false))

(defn to-type [m]
  (case (:record-type m)
    :var :var
    :compound :compound #_(-> m
                  (update :arglist (partial map to-type)))
    :list :list #_(-> m
              (update :head to-type)
              (update :tail to-type))
    :empty-list m
    {:record-type :atomic}))

(defmulti to-string :record-type)
(defmethod to-string :compound [m]
  (str "Compound(" (:functor m) "(" (clojure.string/join ", " (map to-string (:arglist m))) "))"))
(defmethod to-string :list [l]
  (let [elems
        (loop [current-list l
               elements []]
          (let [{head :head tail :tail} current-list]
            (cond
              (= :empty-list (:record-type current-list)) elements
              (= :empty-list (:record-type tail)) (recur tail (conj elements (to-string head)))
              (= :var (:record-type tail)) (-> elements
                                               (conj (to-string head))
                                               (conj "|")
                                               (conj (to-string tail)))
              (= :list (:record-type tail)) (recur tail (-> elements
                                                            (conj (to-string head))
                                                            (conj ", "))))))]
    (str "[" (clojure.string/join elems) "]")))



(defmethod to-string :empty-list [m]
  "[]")

(defmethod to-string :var [m]
  "var")


(defmethod to-string :default [m]
  "atomic")

(defn create-map-for-vars [data]
  (->> data
       (remove string?)
       (map #(select-keys % (non-atomic-keys (keys %))))
       (map #(select-keys % (filter contains-var? (keys %))))
       (map (partial group-by (comp to-type first)))
       (map (partial reduce-kv #(assoc %1 %2 (map second %3)) {}))
       (map #(select-keys % [:var]))
       (mapcat vals)
       (apply concat)
       frequencies
       ))

(defn bla [freq-map]
  (let [total (apply +' (vals freq-map))
        nils (get freq-map nil 0)
        anys (get freq-map {:record-type :any} 0)]
    {:total total :any (+ anys nils)}))

(let [{any :any total :total} (bla (create-map-for-vars x))]
  (float (/ any total)))

(create-map-for-vars x)
