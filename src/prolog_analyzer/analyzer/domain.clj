(ns prolog-analyzer.analyzer.domain
  (:require [prolog-analyzer.utils :as utils]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [ubergraph.core :as uber]
            [loom.graph]
            [loom.attr]
            [ubergraph.protocols]
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




(def built-ins #{:integer :float :number :atomic :atom :ground
                 :nonvar :var :compound :list :tuple :any :named-any :exact})
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


(defn- get-built-in-parent-specs [spec]
  (if (contains? built-ins-relations spec)
    (loop [parent (get built-ins-relations spec)
           ancestors [spec parent]]
      (if-let [grandparent (get built-ins-relations parent)]
        (recur grandparent (conj ancestors grandparent))
        ancestors))
    [spec]))

(defn- is-parent? [parent child]
  (if (some #(= (:spec parent) %) (get-built-in-parent-specs (:spec child))) true false))


(defn built-in? [dom] (contains? built-ins (:spec dom)))

; TODO: currently only working for built-ins
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
                  (every? true? (map is-subdom? (:arglist dom1) (:arglist dom2)))
                  false)
      :any (if (= :any (:spec dom2))
             (or (nil? (:name dom2))
                 (= (:name dom1) (:name dom2)))
             false)
      :exact (if (= :exact (:spec dom2))
               (= (:value dom1) (:value dom2))
               (is-subdom? {:spec :atom} dom2))
      (is-parent? dom2 dom1)
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



(defn simplify-dom [dom]
  (-> dom
      to-knf))


(defn union [dom1 dom2]
  (cond
    (is-subdom? dom1 dom2) dom2
    (is-subdom? dom2 dom1) dom1
    :else                  (simplify-dom {:spec :or :arglist (list dom1 dom2)})))

(defn dom-invalid? [dom]
  (some #(= :error (:spec %)) (flatten (vals dom))))

(declare intersect-doms-aux)
(defn- intersect-with-ground [other-dom]
  (case (:spec other-dom)
    :list               (update other-dom :type (partial intersect-doms-aux {:spec :ground}))
    (:tuple :compound)  (update other-dom :arglist #(map (partial intersect-doms-aux {:spec :ground}) %))
                        other-dom
    ))

(defn- intersect-with-list [{type :type} other-dom]
  (let [new-dom (case (:spec other-dom)
                  :list               (update other-dom :type (partial intersect-doms-aux type))
                  :tuple              (update other-dom :arglist #(map (partial intersect-doms-aux type) %))
                  :ground             {:spec :list :type (intersect-doms-aux other-dom type)}
                  nil
                  )]
    (if (contains? new-dom :type)
      (if (nil? (:type new-dom)) nil new-dom)
      (if (some nil? (:arglist new-dom)) nil new-dom))))

(defn- intersect-with-tuple [{arglist :arglist :as dom1} other-dom]
  (let [new-dom (case (:spec other-dom)
                  :tuple              (if (= (count arglist) (count (:arglist other-dom)))
                                        (update other-dom :arglist #(map intersect-doms-aux arglist %))
                                        nil)
                  :list               (update dom1 :arglist #(map (partial intersect-doms-aux {:type other-dom}) %))
                  :ground             (update dom1 :arglist #(map (partial intersect-doms-aux other-dom) %))
                  nil
                  )]
    (if (some nil? (:arglist new-dom)) nil new-dom)))

(defn- intersect-with-compound [{functor :functor arglist :arglist :as dom1} other-dom]
  (let [new-dom (case (:spec other-dom)
                  :compound           (if (and (= functor (:functor other-dom))
                                               (= (count arglist) (count (:arglist other-dom))))
                                        (update other-dom :arglist #(map intersect-doms-aux arglist %)))
                  :ground             (update dom1 :arglist #(map (partial intersect-doms-aux other-dom) %))
                  nil
                  )]
    (if (some nil? (:arglist new-dom)) nil new-dom)))


(defn- intersect-with-named-any [dom1 dom2]
  {:spec :and :arglist [dom1 dom2]})


(defn- intersect-with-var [dom1 dom2]
  (if (= :var (:spec dom2))
    dom2
    (assoc dom2 :was-var true)))

(defn- intersect-with-exact [{value :value :as dom1} dom2]
  (case (:spec dom2)
    :exact (if (= value (:value dom2)) dom1 nil)
    (:atom :atomic :ground) dom1
    nil)
  )

(defn- intersect-doms-aux [dom1 dom2]
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
    :else nil))


(defn intersect [dom1 dom2]
  (let [dom-intersect (intersect-doms-aux dom1 dom2)]
    (cond
      (nil? dom-intersect) {:spec :error :reason (str "No common subtype of " (:spec dom1) " and " (:spec dom2))}
      (or (:was-var dom1) (:was-var dom2)) (assoc dom-intersect :was-var true)
      :else dom-intersect)))


(defmulti fill-env-for-term-with-spec* (juxt (comp :type first) (comp :spec second)))

(defn add-doms-to-node [env node & doms]
  (if (uber/has-node? env node)
    (uber/set-attrs env node (update (uber/attrs env node) :dom #(concat % doms)))
    (uber/add-nodes-with-attrs env [node {:dom doms}])))


(defn fill-env-for-term-with-spec [env term spec]
  (if (or (= :var (:type term)) (= :anon_var (:type term)))
    (add-doms-to-node env term spec)
    (fill-env-for-term-with-spec* [term spec env])))

(defmethod fill-env-for-term-with-spec* [:atomic :list] [[term spec env]]
  (if (utils/empty-list? term)
    (add-doms-to-node env term spec)
    (add-doms-to-node env term {:spec :error :reason "atomic cannot be a list"})
    ))

(defmethod fill-env-for-term-with-spec* [:atomic :tuple] [[term {arglist :arglist :as spec} env]]
  (if (utils/empty-list? term)
    (if (empty? arglist)
      (add-doms-to-node env term spec)
      (add-doms-to-node env term {:spec :error :reason "empty list cannot be a tuple with non-empty arglist"}))
    (add-doms-to-node env term {:spec :error :reason "atomic cannot be a tuple"})))

(defmethod fill-env-for-term-with-spec* [:atomic :ground] [[term _ env]]
  (if (utils/empty-list? term)
    (add-doms-to-node env term {:spec :list :type {:spec :ground}})
    (add-doms-to-node env term {:spec :atomic})))

(defmethod fill-env-for-term-with-spec* [:atomic :nonvar] [[term _ env]]
  (if (utils/empty-list? term)
    (add-doms-to-node env term {:spec :list :type {:spec :any}})
    (add-doms-to-node env term {:spec :atomic})))

(defmethod fill-env-for-term-with-spec* [:atomic :any] [[term _ env]]
  (if (utils/empty-list? term)
    (add-doms-to-node env term {:spec :list :type {:spec :any}})
    (add-doms-to-node env term {:spec :atomic})))

(defmethod fill-env-for-term-with-spec* [:list :list] [[{head :head tail :tail :as term} {t :type :as spec} env]]
  (-> env
      (add-doms-to-node term spec)
      (fill-env-for-term-with-spec tail spec)
      (fill-env-for-term-with-spec head t)))

(defmethod fill-env-for-term-with-spec* [:list :tuple] [[{head :head tail :tail :as term} {[head-type & rest-types :as r] :arglist :as spec} env]]
  (let [tail-env (case [(utils/empty-list? tail) (empty? rest-types)]
                   [true true] env
                   [true false] (add-doms-to-node env tail {:spec :error :reason (str "empty list cannot be a tuple with non-empty arglist")})
                   [false true] (add-doms-to-node env term {:spec :error :reason "non-empty list cannot be a tuple with no elements"})
                   [false false] (fill-env-for-term-with-spec env tail (update spec :arglist rest)))]
    (-> tail-env
        (fill-env-for-term-with-spec head head-type)
        (add-doms-to-node term spec)))
  )

(defmethod fill-env-for-term-with-spec* [:list :ground] [[{head :head tail :tail :as term} ground-spec env]]
  (let [tail-env (if (utils/empty-list? tail) env (fill-env-for-term-with-spec env tail {:spec :list :type ground-spec}))]
    (-> tail-env
        (fill-env-for-term-with-spec head ground-spec)
        (add-doms-to-node term {:spec :list :type ground-spec})))
  )

(defmethod fill-env-for-term-with-spec* [:list :nonvar] [[{head :head tail :tail :as term} nonvar-spec env]]
  (let [tail-env (if (utils/empty-list? tail) {} (fill-env-for-term-with-spec env tail {:spec :list :type nonvar-spec}))]
    (-> tail-env
        (add-doms-to-node term {:spec :list :type {:type :any}})
        (fill-env-for-term-with-spec head {:spec :any}))))

(defmethod fill-env-for-term-with-spec* [:list :any] [[{head :head tail :tail :as term} any-spec env]]
  (let [tail-env (if (utils/empty-list? tail) {} (fill-env-for-term-with-spec env tail {:spec :list :type any-spec}))]
    (-> tail-env
        (add-doms-to-node term {:spec :list :type {:spec :any}})
        (fill-env-for-term-with-spec head any-spec)))
  )
(defmethod fill-env-for-term-with-spec* [:compound :compound] [[{term-func :functor term-elems :arglist :as term} {spec-func :functor spec-elems :arglist :as spec} env]]
  (cond
    (not= term-func spec-func) (add-doms-to-node env term {:spec :error :reason "functor not identical"})
    (not= (count term-elems) (count spec-elems)) (add-doms-to-node env term {:spec :error :reason "length of argument list not identical"})
    :else (let [pairs (map vector term-elems spec-elems)]
            (reduce #(apply fill-env-for-term-with-spec %1 %2) (add-doms-to-node env term spec) pairs))))

(defmethod fill-env-for-term-with-spec* [:compound :ground] [[{functor :functor arglist :arglist :as term} spec env]]
  (let [term-env (add-doms-to-node env term {:spec :compound :functor functor :arglist (repeat (count arglist) {:spec :ground})})]
    (reduce #(apply fill-env-for-term-with-spec %1 %2) term-env (map vector arglist (repeat spec)))))

(defmethod fill-env-for-term-with-spec* [:compound :nonvar] [[{functor :functor arglist :arglist :as term} _ env]]
  (let [term-env (add-doms-to-node env term {:spec :compound :functor functor :arglist (repeat (count arglist) {:spec :any})})]
    (reduce #(apply fill-env-for-term-with-spec %1 %2) term-env (map vector arglist (repeat {:spec :any})))))

(defmethod fill-env-for-term-with-spec* [:compound :any] [[{functor :functor arglist :arglist :as term} spec env]]
  (let [term-env (add-doms-to-node env term {:spec :compound :functor functor :arglist (repeat (count arglist) {:spec :any})})]
    (reduce #(apply fill-env-for-term-with-spec %1 %2) term-env (map vector arglist (repeat {:spec :any})))))

(defmethod fill-env-for-term-with-spec* :default [[term spec env]]
  (case (:type term)
    :list (add-doms-to-node env term {:spec :error :reason (str "list cannot be of type " (:spec spec))})
    :compound (add-doms-to-node env term {:spec :error :reason (str "compound cannot be of type " (:spec spec))})
    (add-doms-to-node env term (intersect {:spec (:type term)} spec))))
