(ns prolog-analyzer.domain
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]))


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

(defmulti to-knf spec-type)

(defmethod to-knf :normal [dom]
  {:spec :or :arglist [dom]})

(defmethod to-knf :or [dom]
  dom)

(defmethod to-knf :and [dom]
  (if (= 1 (count (:arglist dom)))
    (assoc dom :spec :or)
    (-> dom
        (update :arglist #(->> %
                               (map to-knf)
                               (map :arglist)
                               (reduce (fn [left right]
                                         (for [a left
                                               b right]
                                           {:spec :and :arglist [a b]})))))
        (assoc :spec :or))))



(defn merge-dom [dom1 dom2]
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


(to-knf {:spec :and :arglist [{:spec :or :arglist [{:spec :a} {:spec :b}]} {:spec :or :arglist [{:spec :c} {:spec :d}]}]})
