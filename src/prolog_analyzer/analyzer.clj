(ns prolog-analyzer.analyzer
  (:require
   [prolog-analyzer.parser :refer [process-prolog-file process-prolog-snippets]] ;; used only during development
   ))


(defmulti valid-helper #(:spec (first %)))

(defn valid [spec arg]
  (valid-helper [spec arg]))


(defmethod valid-helper :number [[spec arg]]
  (or (= :number (:type arg))
      (= :integer (:type arg))
      (= :float (:type arg))
      ))

(defmethod valid-helper :atomic [[spec arg]]
  (or
   (= :atomic (:type arg))
   (= :atom (:type arg))
   (valid {:spec :number} arg)
   ))

(defmethod valid-helper :integer [[spec arg]]
  (= :integer (:type arg)))

(defmethod valid-helper :float [[spec arg]]
  (= :float (:type arg)))

(defmethod valid-helper :atom [[spec arg]]
  (= :atom (:type arg)))

(defmethod valid-helper :exact [[{value :value} arg]]
  (and
   (= :atom (:type arg))
   (= value (:term arg))))

(defmethod valid-helper :any [[spec arg]]
  (if (nil? (:name spec))
    true))

(defmethod valid-helper :ground [[spec arg]]
  (or
   (= :ground (:type arg))
   (valid {:spec :atomic} arg)
   (if (= (:type arg) :compound)
     (every? true? (map (partial valid {:spec :ground}) (:arglist arg)))
     false)))

(defmethod valid-helper :nonvar [[spec arg]]
  (and
   (not= :var (:type arg))
   (not= :anon_var (:type arg))))

(defmethod valid-helper :var [[spec arg]]
  (or
   (= :var (:type arg))
   (= :anon_var (:type arg))))

(defmethod valid-helper :list [[{inner-type :type} arg]]
  (if (= :head-tail-list (:type arg))
    false
    (every? (partial valid inner-type) (:arglist arg))))

(defmethod valid-helper :tuple [[spec arg]]
  (if (= :list (:type arg))
    (every? true? (map valid (:arglist spec) (:arglist arg)))
    false))

(defmethod valid-helper :one_of [[spec arg]]
  (not (not-any? true? (for [x (:arglist spec)]
                         (valid x arg)))))

(defmethod valid-helper :and [[spec arg]]
  (every? true (for [x (:arglist spec)]
                 (valid x arg))))

(defmethod valid-helper :compound [[{spec-functor :functor spec-arglist :arglist} {arg-functor :functor arg-arglist :arglist}]]
  (and (= spec-functor arg-functor)
       (every? true? (map valid spec-arglist arg-arglist))))

(defmethod valid-helper :default [[spec arg]]
  (println (str "default: " spec)))

(defn get-specs-of-pred [pred-identity data]
  (let [spec-identity (rest pred-identity)]
    (-> data
        (select-keys [:pre-specs :post-specs :inv-specs])
        (update :pre-specs #(get-in % spec-identity))
        (update :post-specs #(get-in % spec-identity))
        (update :inv-specs #(get-in % spec-identity))
        )))

(defn get-instances-of-pred [pred-identity data]
  (get-in data (apply vector :preds pred-identity)))

(defn get-pred-identities [data]
  (for [module (keys (:preds data))
        pred-name (keys (get-in data [:preds module]))
        arity (keys (get-in data [:preds module pred-name]))]
    [module pred-name arity]))

(defn get-initial-domain-of-arg [arg]
  (case (:type arg)
    :head-tail-list :list
    :anon_var :var
    (:type arg)
    ))

(defn get-start-domains [pred-key data]
  (let [pred-impls (get-instances-of-pred pred-key data)]
    pred-impls)
  )


(defmulti domain-of :type)
(defmethod domain-of :anon_var [arg]
  (assoc arg :dom {:spec :var}))

(defmethod domain-of :head-tail-list [arg]
  (let [inner-domains (-> arg
                          (update :head domain-of)
                          (update :tail domain-of))
        dom-head (get-in inner-domains [:head :dom])
        dom-tail (get-in inner-domains [:tail :dom])]
    (assoc inner-domains :dom {:spec :and :arglist '(dom-head dom-tail)})))

(defmethod domain-of :default [{type :type}]
  {:spec type})
