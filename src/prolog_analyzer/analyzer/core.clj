(ns prolog-analyzer.analyzer.core
  (:require
   [prolog-analyzer.parser :refer [process-prolog-file]] ;; used only during development
   [prolog-analyzer.analyzer.domain :as dom]
   [prolog-analyzer.analyzer.relationship-analyzer :as rel]
   [prolog-analyzer.records :as r]
   [prolog-analyzer.utils :as utils]
   [prolog-analyzer.analyzer.pretty-printer :as my-pp]
   [ubergraph.core :as uber]
   [ubergraph.protocols]
   [loom.graph]
   [loom.attr]
   [clojure.set]
   [clojure.pprint :as pp]
   [clojure.tools.logging :as log]
   [clojure.tools.namespace.repl :refer [refresh]]
   ))

(defmulti replace-specvars-with-uuid (comp sequential? first))
(defmethod replace-specvars-with-uuid false [pre-spec]
  (let [specvars (->> pre-spec
                      (reduce #(concat %1 (r/find-specvars %2)) #{})
                      set
                      (map :name))
        ids (repeatedly gensym)
        uuid-map (apply hash-map (interleave specvars ids))]
    (map #(reduce-kv r/replace-specvar-name-with-value % uuid-map) pre-spec)))

(defmethod replace-specvars-with-uuid true [[condition premise :as post-spec]]
  (let [specvars (->> premise
                      (conj condition)
                      (reduce #(concat %1 (r/find-specvars %2)) #{})
                      (map :name)
                      set)
        ids (repeatedly gensym)
        uuid-map (apply hash-map (interleave specvars ids))
        new-condition (map #(reduce-kv r/replace-specvar-name-with-value % uuid-map) condition)
        new-premise (reduce-kv r/replace-specvar-name-with-value premise uuid-map)]
    [new-condition new-premise]))

(defmulti add-relationships-aux (fn [env term] (type term)))
(defmethod add-relationships-aux prolog_analyzer.records.ListTerm [env {head :head tail :tail :as term}]
  (if (r/empty-list? tail)
    (-> env
        (dom/fill-env-for-term-with-spec head (r/->AnySpec))
        (uber/add-edges [head term {:relation :is-head}]))
    (-> env
        (dom/fill-env-for-term-with-spec head (r/->AnySpec))
        (dom/fill-env-for-term-with-spec tail (r/->ListSpec (r/->AnySpec)))
        (uber/add-edges [head term {:relation :is-head}] [tail term {:relation :is-tail}]))))

(defmethod add-relationships-aux prolog_analyzer.records.CompoundTerm [env {functor :functor arglist :arglist :as term}]
  (apply
   uber/add-edges
   (dom/multiple-fills env arglist (repeat (count arglist) (r/->AnySpec)))
   (map-indexed #(vector %2 term {:relation :arg-at-pos :pos %1}) arglist)))


(defn- remove-nil-doms [env]
  (->> env
       utils/get-terms
       (filter #(nil? (utils/get-dom-of-term env %)))
       (reduce #(dom/add-type-to-dom %1 %2 (r/->AnySpec)) env)))

(defmethod add-relationships-aux :default [env _]
  env)

(defn add-relationships [env]
  ;(log/debug "Add Relationships")
  (reduce #(add-relationships-aux %1 %2) env (utils/get-terms env)))

(defn- goal-args->tuple [arglist]
  (apply r/to-head-tail-list arglist))

(defn- goal-specs->tuples [goal-specs]
  (map (partial apply r/to-tuple-spec) goal-specs))


(defn evaluate-goal-pre-specs [env {goal-name :goal module :module arity :arity arglist :arglist :as goal} data]
  (let [goal-specs (some->> data
                            (utils/get-specs-of-pred [module goal-name arity])
                            (:pre-specs)
                            (map replace-specvars-with-uuid))
        term (goal-args->tuple arglist)
        goal-specs-as-tuples (goal-specs->tuples goal-specs)]
    (if (and (> arity 0) goal-specs)
      (dom/fill-env-for-term-with-spec env term (apply r/to-or-spec (:specs data) goal-specs-as-tuples))
      env)))

(defn- valid? [env term spec]
  (let [old-dom (utils/get-dom-of-term env term)
        new-dom (-> env
                    (dom/fill-env-for-term-with-spec term spec)
                    (utils/get-dom-of-term term))]
    (= old-dom new-dom)))

(defn condition-fullfilled? [env {head :head tail :tail :as head-tail-list} [conditions p]]
  (if (and (r/empty-list? head-tail-list) (empty? conditions))
    true
    (and (valid? env head (first conditions))
         (condition-fullfilled? env tail [(rest conditions) p]))))


(defn apply-valid-post-spec [tuple-term env post-spec]
  (let [[condition promise :as with-uuid] (replace-specvars-with-uuid post-spec)]
    (if (condition-fullfilled? env tuple-term with-uuid)
      (-> env
          (dom/fill-env-for-term-with-spec tuple-term (apply r/to-tuple-spec condition) {:initial :false :overwrite false})
          (dom/fill-env-for-term-with-spec tuple-term promise {:initial false :overwrite true}))
      env)))

(defn evaluate-goal-post-specs [env {goal-name :goal module :module arity :arity arglist :arglist :as goal} data]
  (let [goal-specs (some->> data
                            (utils/get-specs-of-pred [module goal-name arity])
                            (:post-specs)
                            (map replace-specvars-with-uuid))
        term (goal-args->tuple arglist)
        ]
    (if (empty? goal-specs)
      env
      (reduce (partial apply-valid-post-spec term) env goal-specs))))

(defmulti evaluate-goal-relationships (fn [env goal data] (:goal goal)))

(defmethod evaluate-goal-relationships :default [env goal data]
  env)



(defn set-indices [env {goal-name :goal arglist :arglist} data]
  (reduce-kv
   (fn [e index arg] (if (and (uber/has-node? e arg) (uber/attr e arg :indices))
                      (uber/add-attr e arg :indices (assoc (uber/attr e arg :indices) goal-name index))
                      (-> e
                          (uber/add-nodes arg)
                          (uber/add-attr arg :indices {goal-name index}))))
   env
   (apply vector arglist)))

(defn evaluate-goal [data env goal]
  (-> env
      (evaluate-goal-pre-specs goal data)
      (evaluate-goal-post-specs goal data)
      (evaluate-goal-relationships goal data)
      (set-indices goal data)))

(defn evaluate-body [env data body]
  ;(log/debug "Evaluate Body")
  (reduce (partial evaluate-goal data) env body))

(defn- add-index-to-input-arguments [env arglist]
  (apply uber/add-nodes-with-attrs env (map-indexed #(vector %2 {:index %1}) arglist)))


(defn initial-env [data arglist pre-spec]
  ;(log/debug "Initialize Env")
  (-> (uber/digraph)
      (uber/add-nodes-with-attrs [:ENVIRONMENT {:user-defined-specs (get data :specs)}])
      (dom/fill-env-for-term-with-spec (apply r/to-head-tail-list arglist) pre-spec {:initial true})
      (add-index-to-input-arguments arglist)
      ))

(defn analyzing [data {arglist :arglist body :body :as clause} pre-spec pred-id]
  (-> (initial-env data arglist pre-spec)
      (uber/add-attr :ENVIRONMENT :pred-id pred-id)
      (evaluate-body data body)
      add-relationships
      remove-nil-doms
      rel/fixpoint-analysis
      ))

(defn complete-analysis [data]
  ;(log/debug "Start analysis")
  (when (empty? (utils/get-pred-identities data))
    (println (pr-str "No predicates found")))
  (for [pred-id (utils/get-pred-identities data)
        clause-number (utils/get-clause-identities-of-pred pred-id data)]
    (let [pre-spec (r/simplify-or
                    (->> (utils/get-specs-of-pred pred-id data)
                         :pre-specs
                         (map replace-specvars-with-uuid)
                         (map r/->TupleSpec)
                         set
                         r/->OneOfSpec)
                    (:specs data))]
      (analyzing data (utils/get-clause pred-id clause-number data) pre-spec (conj pred-id clause-number)))))

(defn complete-analysis-parallel [data]
  (when (empty? (utils/get-pred-identities data))
    (println (pr-str "No predicates found")))
  (let [tasks (for [pred-id (utils/get-pred-identities data)
                    clause-number (utils/get-clause-identities-of-pred pred-id data)]
                (let [pre-spec (r/simplify-or
                                (->> (utils/get-specs-of-pred pred-id data)
                                     :pre-specs
                                     (map replace-specvars-with-uuid)
                                     (map r/->TupleSpec)
                                     set
                                     r/->OneOfSpec)
                                (:specs data))]
                  {:clause (utils/get-clause pred-id clause-number data) :pre-spec pre-spec :title (conj pred-id clause-number)}
                  ))]
    (pmap (fn [{clause :clause pre-spec :pre-spec title :title}] (analyzing data clause pre-spec title)) tasks)))


(defn- create-post-spec [env]
  (let [indexed-terms (->> env
                           utils/get-terms
                           (filter #(uber/attr env % :index)))
        premise (->> indexed-terms
                     (map #(uber/attrs env %))
                     (sort-by :index)
                     (map :dom)
                     (apply r/to-tuple-spec))
        condition (->> indexed-terms
                       (map #(assoc (uber/attrs env %) :pre (r/maybe-spec (r/initial-spec %))))
                       (sort-by :index)
                       (map :pre)
                       (apply vector))]
    {condition premise}))

(defn- create-post-spec-premise [env]
  (->> env
       utils/get-terms
       (filter #(uber/attr env % :index))
       (map #(uber/attrs env %))
       (sort-by :index)
       (map :dom)
       (apply r/to-tuple-spec)))


(defn- valid-env? [env]
  (->> env
       utils/get-terms
       (map (partial utils/get-dom-of-term env))
       (every? (complement r/error-spec?))))


(defn process-predicate-envs [data pred-id envs]
  (if (every? valid-env? envs)
    (let [post-specs-map (apply merge-with #(r/simplify-or (r/->OneOfSpec (hash-set %1 %2)) (get data :spec)) (map create-post-spec envs))]
      (update-in data [:post-specs pred-id] (partial merge-with #(r/simplify-and-without-intersect (r/->AndSpec (hash-set %1 %2))) post-specs-map)))
    data))


(defn- add-new-knowledge [data envs]
  (->> envs
       (group-by #(apply vector (drop-last (uber/attr % :ENVIRONMENT :pred-id))))
       (reduce-kv process-predicate-envs data)))


(defn global-analysis [data]
  (let [envs (complete-analysis-parallel data)
        new-data (add-new-knowledge data envs)]
    (if (= data new-data)
      envs
      (global-analysis new-data))))
