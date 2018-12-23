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




(def built-ins #{:integer :float :number :atomic :atom :ground :nonvar :var :compound :list :tuple :any :named-any :exact})
(def built-ins-relations
  {:integer :number
   :float :number
   :number :atomic
   :atom :atomic
   :atomic :ground
   :ground :nonvar 
   :var :any
   :named-any :any
   :nonvar :any
   :exact :atom})


(defn get-built-in-parent-specs [spec]
  (if (contains? built-ins-relations spec)
    (loop [parent (get built-ins-relations spec)
           ancestors [spec parent]]
      (if-let [grandparent (get built-ins-relations parent)]
        (recur grandparent (conj ancestors grandparent))
        ancestors))
    [spec]))

(defn built-in? [dom] (contains? built-ins (:spec dom)))

(defn is-subdom? [dom1 dom2]
  (if (built-in? dom1)
    (case (:spec dom1)
      :list (if (= :list (:spec dom2))
              (is-subdom? (:type dom1) (:type dom2))
              false)
      :tuple (if (and
                  (= :tuple (:spec dom2))
                  (= (count (:arglist dom1)) (count (:arglist dom2))))
               (every? true? (map is-subdom? (:arglist dom1) (:arglist dom2))) false)
      :compound (if (and
                     (= (:functor dom1) (:functor dom2))
                     (= (count (:arglist dom1)) (count (:arglist dom2))))
                  (every? true? (map is-subdom? (:arglist dom1) (:arglist dom2))) false)
      :any (if (= :any (:spec dom2))
             (or (nil? (:name dom2))
                 (= (:name dom1) (:name dom2)))
             false)
      :exact (if (= :exact (:spec dom2))
               (= (:value dom1) (:value dom2))
               (is-subdom? {:spec :atom} dom2))
      (if (some #(= (:spec dom2) %) (get-built-in-parent-specs (:spec dom1)))
        true
        false)
      )
    false))

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


(s/fdef simplify-dom
        :args (s/cat :dom ::dom)
        :ret (s/and ::dom)
        :fn #(knf? (:ret %)))
(defn simplify-dom [dom]
  (-> dom
      to-knf))
(declare intersect)
(defn union [dom1 dom2]
  (cond
    (is-subdom? dom1 dom2) dom2
    (is-subdom? dom2 dom1) dom1
    :else (simplify-dom {:spec :or :arglist (list dom1 dom2)})))

(defn- intersect-with-ground [other-dom]
  (case (:spec other-dom)
    :list (update other-dom :type (partial intersect {:spec :ground}))
    (:tuple :compound) (update other-dom :arglist #(map (partial intersect {:spec :ground}) %))
    other-dom))

(defn- intersect-with-list [{type :type} other-dom]
  (case (:spec other-dom)
    :list (update other-dom :type (partial intersect type))
    :tuple (update other-dom :arglist #(map (partial intersect type) %))
    :ground {:spec :list :type (intersect other-dom type)}
    nil))

(defn- intersect-with-tuple [{arglist :arglist :as dom1} other-dom]
  (case (:spec other-dom)
    :tuple (update other-dom :arglist #(map intersect arglist %))
    :list (update dom1 :arglist #(map (partial intersect {:type other-dom}) %))
    :ground (update dom1 :arglist #(map (partial intersect other-dom) %))
    nil))

(defn- intersect-with-compound [{functor :functor arglist :arglist :as dom1} other-dom]
  (case (:spec other-dom)
    :compound (if (and (= functor (:functor other-dom))
                       (= (count arglist) (count (:arglist other-dom))))
                (update other-dom :arglist #(map intersect arglist %)))
    :ground (update dom1 :arglist #(map (partial intersect other-dom) %))
    nil))

(defn- intersect-with-named-any [dom1 dom2]
  {:spec :and :arglist [dom1 dom2]})


(defn intersect-with-var [dom1 dom2]
  (if (= :var (:spec dom2))
    dom2
    (assoc dom2 :was-var true)))

(defn intersect-with-exact [{value :value :as dom1} dom2]
  (case (:spec dom2)
    :exact (if (= value (:value dom2)) dom1 nil)
    (:atom :atomic :ground) dom1
    nil)
  )

(defn intersect-doms [dom1 dom2]
  (cond
    (is-subdom? dom1 dom2) dom1
    (is-subdom? dom2 dom1) dom2
    (= :any (:spec dom1)) dom2
    (= :any (:spec dom2)) dom1
    (= :named-any (:spec dom1)) (intersect-with-named-any (:name dom1) dom2)
    (= :named-any (:spec dom2)) (intersect-with-named-any (:name dom2) dom1)
    (= :var (:spec dom1)) (intersect-with-var dom1 dom2)
    (= :var (:spec dom2)) (intersect-with-var dom2 dom1)
    (= :nonvar (:spec dom1)) dom2
    (= :nonvar (:spec dom2)) dom1
    (= :exact (:spec dom1)) (intersect-with-exact dom1 dom2)
    (= :exact (:spec dom2)) (intersect-with-exact dom2 dom1)

    (= :ground (:spec dom1)) (intersect-with-ground dom2)
    (= :list (:spec dom1)) (intersect-with-list dom1 dom2)
    (= :tuple (:spec dom1)) (intersect-with-tuple dom1 dom2)
    (= :compound (:spec dom1)) (intersect-with-compound dom1 dom2)
    :else (simplify-dom {:spec :and :arglist (list dom1 dom2)})))

(defn intersect [dom1 dom2]
  (let [dom-intersect (intersect-doms dom1 dom2)]
    (if (or (:was-var dom1) (:was-var dom2))
      (assoc dom-intersect :was-var true)
      dom-intersect)))

(defn merge-dom [dom1 dom2]
  (intersect dom1 dom2))
