(ns prolog-analyzer.domain
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            ))


(s/def :normal/spec (s/spec #{:other}))
(s/def :or/spec (s/spec #{:or}))
(s/def :and/spec (s/spec #{:and}))
(s/def ::arglist (s/coll-of ::dom))



(s/def ::dom (s/or :normal ::normal-spec
                   :or ::or-spec
                   :and ::and-spec))

(s/def ::normal-spec (s/keys :req-un [:normal/spec]))
(s/def ::or-spec (s/keys :req-un [:or/spec ::arglist]))
(s/def ::and-spec (s/keys :req-un [:and/spec ::arglist]))



(s/fdef is-subdom?
        :args (s/cat :dom1 ::dom :dom2 ::dom)
        :ret boolean?)
(defn is-subdom? [dom1 dom2] false)



(defn spec-type [dom]
  (case (:spec dom)
    :and :and
    :or :or
    :normal))

(defn- knf-inner? [dom]
  (case (spec-type dom)
    :normal true
    :and (every? knf-inner? (:arglist dom))
    :or false))

(defn knf? [dom]
  (case (spec-type dom)
    :normal true
    :and false
    :or (every? knf-inner? (:arglist dom))))

(defmulti to-knf spec-type)

(defmethod to-knf :normal [dom]
  dom)

(defmethod to-knf :or [dom]
  (if (= 1 (count (distinct (:arglist dom))))
    (to-knf (first (:arglist dom)))
    (-> dom
        (update :arglist distinct)
        (update :arglist (partial map to-knf))
        (update :arglist (partial mapcat #(get % :arglist (list %))))
        (update :arglist distinct)
        )))

(defn and-two-doms [a b]
  (let [new-dom (cond
                  (and (= :and (:spec a)) (= :and (:spec b))) (update a :arglist (partial concat (:arglist b)))
                  (= :and (:spec a)) (update a :arglist #(conj % b))
                  (= :and (:spec b)) (update b :arglist #(conj % a)))]
    (update new-dom :arglist distinct)
    ))


(defmethod to-knf :and [dom]
  (if (= 1 (count (distinct (:arglist dom))))
    (to-knf (first (:arglist dom)))
    (-> dom
        (update :arglist #(->> %
                               (map to-knf)
                               (map (fn [dom] (get dom :arglist [dom])))
                               (reduce (fn [left right]
                                         (for [a left
                                               b right]
                                           (cond
                                             (= a b) a
                                             (or (= :and (:spec a)) (= :and (:spec b))) (and-two-doms a b)
                                             :else {:spec :and :arglist [a b]}))))))
        (update :arglist distinct)
        (assoc :spec :or))))

(binding [s/*recursion-limit* 1] (gen/generate (s/gen (s/and ::dom knf?))))

(s/fdef simplify-knf
        :args (s/cat :dom ::dom)
        :ret (s/and ::dom)
        :fn #(knf? (:ret %)))

(defn simplify-dom [dom]
  (-> dom
      to-knf)
  )



(defn merge-dom [dom1 dom2]
  (simplify-dom {:spec :and :arglist (list dom1 dom2)}))


(defn union [dom1 dom2]
  (cond
    (is-subdom? dom1 dom2) dom2
    (is-subdom? dom2 dom1) dom1
    :else (simplify-dom {:spec :or :arglist (list dom1 dom2)})))


(defn intersect [dom1 dom2]
  (cond
    (is-subdom? dom1 dom2) dom1
    (is-subdom? dom2 dom1) dom2
    :else (simplify-dom {:spec :and :arglist (list dom1 dom2)})))


