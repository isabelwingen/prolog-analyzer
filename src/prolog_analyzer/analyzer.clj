(ns prolog-analyzer.analyzer
  (:require
   [prolog-analyzer.parser :refer [process-prolog-file process-prolog-snippets]] ;; used only during development
   [prolog-analyzer.domain]
   [clojure.set]
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

(def data (atom (process-prolog-snippets ":- spec_pre(foo/2,[list(int),int]). foo([],_) :- !. foo([E|T],E) :- foo(T,E).")))

(defn id [arg]
  (hash arg))


(defn merge-into-env [env arg new-value]
  (if (contains? env (id arg))
    (-> env
        (update-in [(id arg) :dom] (partial prolog-analyzer.domain/merge-dom (:dom new-value)))
        (update-in [(id arg) :relations] (partial clojure.set/union (:relations new-value))))
    (-> env
        (assoc-in [:id-mapping (id arg)] arg)
        (assoc (id arg) new-value))))

(defmulti add-to-env-aux #(:type (second %)))
(defmethod add-to-env-aux :head-tail-list [[env {head :head tail :tail :as arg}]]
  (-> env
      (merge-into-env arg {:dom {:spec :list :type {:spec :any}} :relations #{{:head (id head)} {:tail (id tail)}}})
      (merge-into-env head {:dom {:spec :any} :relations #{{:head-of (id arg)}}})
      (merge-into-env tail {:dom {:spec :list :type {:spec :any}} :relations #{{:tail-of (id arg)}}})))

(defmethod add-to-env-aux :default [[env arg]]
  (merge-into-env env arg {:dom {:spec :any} :relations #{}}))

(defn add-to-env [env arg]
  (let [new-env (assoc-in env [:id-mapping (id arg)] arg)]
    (add-to-env-aux [new-env arg])))



(defn analyzing [{arglist :arglist body :body} pre-spec]
  (let [env {:id-mapping {} :args (zipmap (range 0 (count arglist)) (map id arglist))}]
    (reduce add-to-env env arglist)))


;; utils
(defn get-specs-of-pred [pred-identity]
  (let [spec-identity (rest pred-identity)
        specs @data]
    (-> specs
        (select-keys [:pre-specs :post-specs :inv-specs])
        (update :pre-specs #(get-in % spec-identity))
        (update :post-specs #(get-in % spec-identity))
        (update :inv-specs #(get-in % spec-identity))
        )))

(defn get-impls-of-pred [pred-identity]
  (get-in @data (apply vector :preds pred-identity)))

(defn get-pred-identities []
  (for [module (keys (:preds @data))
        pred-name (keys (get-in @data [:preds module]))
        arity (keys (get-in @data [:preds module pred-name]))]
    [module pred-name arity]))

(defn get-initial-domain-of-arg [arg]
  (case (:type arg)
    :head-tail-list :list
    :anon_var :var
    (:type arg)
    ))

(defn complete-analysis [data]
  (for [pred-id (get-pred-identities)
        impl (get-impls-of-pred pred-id)
        pre-spec (:pre-specs (get-specs-of-pred pred-id))]
    (analyzing impl pre-spec)))

(complete-analysis data)


