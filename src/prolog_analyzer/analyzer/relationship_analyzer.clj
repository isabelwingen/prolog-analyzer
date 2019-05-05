(ns prolog-analyzer.analyzer.relationship-analyzer
  (:require [ubergraph.core :as uber]
            [prolog-analyzer.analyzer.domain :as dom]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.utils :as utils :refer [case+]]
            [clojure.tools.logging :as log]
            [loom.attr]
            [loom.graph]
            ))



(defmulti process-edge (fn [env edge] (uber/attr env edge :relation)))

(defmethod process-edge :specvar [env edge]
  (let [specvar (uber/dest edge)
        term (uber/src edge)
        specvar-dom (utils/get-dom-of-term env specvar (r/->AnySpec))
        term-dom (utils/get-dom-of-term env term (r/->AnySpec))]
    (-> env
        (dom/add-type-to-dom specvar term-dom {:overwrite true})
        (dom/add-type-to-dom term specvar-dom {:overwrite true}))))

(defn- mark-as-changed [env edge]
  (if-let [counter (uber/attr env edge :changed)]
    (-> env
        (uber/remove-attr edge :changed)
        (uber/add-attr edge :changed (inc counter)))
    (uber/add-attr env edge :changed 0)))

(defn- reset-union [env edge]
  (let [specvar (uber/dest edge)
        term (uber/src edge)
        others (-> env (uber/attrs specvar) (dissoc :dom) (dissoc :compatible) (dissoc term) vals)
        new-dom (-> (apply hash-set (utils/get-dom-of-term env term) others)
                    r/->OneOfSpec
                    (r/simplify-or (utils/get-user-defined-specs env)))]
    (-> env
        (uber/remove-attr specvar :dom)
        (uber/add-attr specvar :dom new-dom)
        (uber/remove-attr specvar :compatible)
        (uber/add-attr specvar :compatible new-dom)
        (uber/remove-attr specvar term)
        (uber/add-attr specvar term (utils/get-dom-of-term env term))
        (mark-as-changed edge))))

(defmethod process-edge :union [env edge]
  (let [specvar (uber/dest edge)
        term (uber/src edge)
        term-dom (utils/get-dom-of-term env term)
        before-dom (uber/attr env specvar term)
        specvar-dom (utils/get-dom-of-term env specvar term-dom)
        new-dom (r/simplify-or (r/->OneOfSpec (hash-set term-dom specvar-dom)) (utils/get-user-defined-specs env))]
    (cond (nil? term-dom)            env
          (nil? before-dom)          (-> env
                                         (uber/remove-attr specvar :dom)
                                         (uber/add-attr specvar :dom new-dom)
                                         (uber/remove-attr specvar term)
                                         (uber/add-attr specvar term term-dom))
          (not= before-dom term-dom)  (reset-union env edge)
          :else                       env)))


(defmethod process-edge :compatible [env edge]
  (let [specvar (uber/dest edge)
        term (uber/src edge)
        specvar-dom (utils/get-dom-of-term env specvar (r/->AnySpec))
        compatible (or (uber/attr env specvar :compatible) specvar-dom)
        step1 (-> env
                  (dom/add-type-to-dom term compatible {:overwrite true})
                  (dom/add-type-to-dom term specvar-dom {:overwrite true}))
        new-term-dom (utils/get-dom-of-term step1 term (r/->AnySpec))]
    (-> step1
        (uber/remove-attr specvar :compatible)
        (uber/add-attr specvar :compatible new-term-dom))))

(defn find-placeholders [spec]
  (case+ (r/spec-type spec)
         r/PLACEHOLDER [spec]
         (r/OR, r/AND, r/COMPOUND, r/TUPLE) (set (reduce concat (map find-placeholders (.arglist spec))))
         r/USERDEFINED (if (contains? spec :arglist) (set (reduce concat (map find-placeholders (:arglist spec)))) [])
         r/LIST (find-placeholders (.type spec))
         #{}))


(defn- process-unions-in-complex-userdef [env edge-id type-map]
  (let [unions (->> type-map
                    (group-by :inner-spec)
                    (reduce-kv #(assoc %1 (r/->SpecvarSpec (:name %2)) (map :alias %3)) {})
                    (reduce-kv #(assoc %1 %2 (r/simplify-or (r/->OneOfSpec (set %3)) (utils/get-user-defined-specs env))) {}))
        name-mapping (reduce #(assoc %1 %2 (r/->VarTerm (str edge-id "_" (:name %2)))) {} (keys unions))]
    (reduce-kv
     #(-> %1
          (uber/add-nodes %3)
          (uber/remove-attr %3 :dom)
          (uber/add-attr %3 :dom (get unions %2))
          (uber/add-edges [%3 %2 {:relation :union}]))
     env
     name-mapping))
  )

(defn- process-compas-in-complex-userdef [env edge-id type-map]
  (let [compas (->> type-map
                    (group-by :inner-spec)
                    (reduce-kv #(assoc %1 (r/->SpecvarSpec (:name %2)) (map :alias %3)) {})
                    (reduce-kv #(assoc %1 %2 (r/simplify-and (r/->AndSpec (set %3)) (utils/get-user-defined-specs env) {:overwrite true})) {}))
        name-mapping (reduce #(assoc %1 %2 (r/->VarTerm (str edge-id "_" (:name %2)))) {} (keys compas))]
    (reduce-kv
     #(-> %1
          (uber/add-nodes %3)
          (uber/remove-attr %3 :dom)
          (uber/add-attr %3 :dom (get compas %2))
          (uber/add-edges [%3 %2 {:relation :compatible}]))
     env
     name-mapping))
  )

(defmethod process-edge :complex-userdef [env edge]
  (let [userdef-spec (r/replace-union-and-comp-with-placeholder (uber/dest edge))
        edge-id (or (uber/attr env edge :id) (gensym "ID_"))
        term (uber/src edge)
        intersect (r/intersect (utils/get-dom-of-term env term (r/->AnySpec)) userdef-spec (utils/get-user-defined-specs env))
        placeholders (map #(if (:alias %) % (assoc % :alias (r/->AnySpec))) (find-placeholders intersect))
        {us :union cs :compatible} (group-by #(r/spec-type (:inner-spec %)) placeholders)
        ]
    (-> env
        (uber/add-attr edge :id edge-id)
        (process-unions-in-complex-userdef edge-id us)
        (process-compas-in-complex-userdef edge-id cs)
        )))


(defmethod process-edge :artificial [env edge]
  (let [normal (uber/src edge)
        artifical (uber/dest edge)]
    (-> env
        (dom/fill-env-for-term-with-spec normal (utils/get-dom-of-term env artifical))
        (dom/fill-env-for-term-with-spec artifical (utils/get-dom-of-term env normal)))))

(defmethod process-edge :is-head [env edge]
  (let [list (uber/dest edge)
        head (uber/src edge)
        head-dom (utils/get-dom-of-term env head)
        tail (some->> list
                      (uber/in-edges env)
                      (filter #(= :is-tail (uber/attr env % :relation)))
                      first
                      uber/src)
        tail-dom (or (utils/get-dom-of-term env tail) (r/->AnySpec))
        overwrite true]
    (if (nil? tail)
      (dom/fill-env-for-term-with-spec env list (r/->TupleSpec [head-dom]) {:overwrite overwrite})
      (case+ (r/spec-type tail-dom)
             r/TUPLE (dom/fill-env-for-term-with-spec env list (update tail-dom :arglist #(cons head-dom %)) {:overwrite overwrite})
             r/LIST (dom/fill-env-for-term-with-spec env list (update tail-dom :type #(r/->OneOfSpec (hash-set % head-dom))) {:overwrite overwrite})
             env))))

(defn- extract-type [env spec]
  (case+ (r/spec-type spec)
         r/LIST (.type spec)
         r/TUPLE (r/simplify-or (r/->OneOfSpec (.arglist spec)) (utils/get-user-defined-specs env))
         (r/->AnySpec)))

(defmethod process-edge :has-type [env edge]
  (let [list (uber/src edge)
        list-dom (utils/get-dom-of-term env list (r/->AnySpec))
        new-type-dom (extract-type env list-dom)
        list-type (uber/dest edge)
        type-dom (utils/get-dom-of-term env list-type (r/->AnySpec))]
    (-> env
        (dom/fill-env-for-term-with-spec list-type new-type-dom)
        (dom/fill-env-for-term-with-spec list (r/->ListSpec type-dom)))))


(defmethod process-edge :arg-at-pos [env edge]
  (let [compound (uber/dest edge)
        part (uber/src edge)
        part-dom (utils/get-dom-of-term env part)
        compound-dom (or (utils/get-dom-of-term env compound) (r/->AnySpec))
        pos (uber/attr env edge :pos)]
    (if (= r/COMPOUND (r/spec-type compound-dom))
      (if (>= pos (count (:arglist compound-dom)))
        (do
          (println (pr-str "ERROR found: Found a compound with size " (count (:arglist compound-dom)) " but need at least size " (inc pos)))
          env)
        (dom/fill-env-for-term-with-spec env compound (update compound-dom :arglist #(assoc (apply vector %) pos part-dom)) {:overwrite true}))
      env)))

(defmethod process-edge :default [env edge]
  env)


(defn- same [env other-env]
  (= env other-env))

(-> (uber/digraph)
    (uber/add-nodes :a :b)
    (uber/add-edges [:a :b])
    (uber/remove-nodes :b)
    (uber/edges))

(defn- apply-edges [env edges]
  (reduce process-edge env edges))

(defn- step [env]
  (let [{unions :union compatibles :compatible :as m} (group-by #(uber/attr env % :relation) (uber/edges env))
        other-edges (-> m (dissoc :union) (dissoc :compatible) vals flatten)]
    (-> env
        (apply-edges other-edges)
        (apply-edges unions)
        (apply-edges compatibles))))

(defn clean-up [env]
  (let [specvar-nodes (->> env
                           uber/edges
                           (filter #(or (= :union (uber/attr env % :relation))
                                        (= :compatible (uber/attr env % :relation))))
                           (map uber/dest)
                           set)]
    (reduce
     #(uber/set-attrs %1 %2 (select-keys (uber/attrs %1 %2) [:dom :compatible]))
     env
     specvar-nodes)))


(defn fixpoint-analysis [env]
  (clean-up
   (loop [in env
          counter 0]
     (let [next (step in)]
       (if (same in next)
          next
         (if (> counter 50)
           (do
             (println (pr-str "Infinite loop at " (uber/attr env :ENVIRONMENT :pred-id)))
             next)
           (recur next (inc counter))))))))
