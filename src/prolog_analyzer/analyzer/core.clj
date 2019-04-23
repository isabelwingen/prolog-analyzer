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

(defn replace-specvars-with-uuid [pre-spec]
  (let [specvars (->> pre-spec
                      (reduce #(concat %1 (r/find-specvars %2)) [])
                      distinct
                      (map :name))
        ids (repeatedly (count specvars) gensym)
        uuid-map (apply hash-map (interleave specvars ids))]
    (map #(reduce-kv r/replace-specvar-name-with-value % uuid-map) pre-spec)))

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
        goal-specs-as-tuples (goal-specs->tuples goal-specs)
        ]
    (if (and (> arity 0) goal-specs)
      (dom/fill-env-for-term-with-spec env term (apply r/to-or-spec (:specs data) goal-specs-as-tuples))
      env)))

(defn condition-fullfilled? [env {head :head tail :tail :as head-tail-list} [conditions p]]
  (if (and (r/empty-list? head-tail-list) (empty? conditions))
    true
    (and (dom/spec-valid? env head (first conditions))
         (condition-fullfilled? env tail [(rest conditions) p]))))


(defn apply-valid-post-spec [env {arglist :arglist :as tuple-term} [condition promise :as post-spec]]
  (if (condition-fullfilled? env tuple-term post-spec)
    (dom/fill-env-for-term-with-spec env tuple-term (apply r/to-tuple-spec promise) {:initial false :overwrite true})
    env))

(defn evaluate-goal-post-specs [env {goal-name :goal module :module arity :arity arglist :arglist :as goal} data]
  (let [goal-specs (some->> data
                            (utils/get-specs-of-pred [module goal-name arity])
                            (:post-specs)
                            (map (partial map replace-specvars-with-uuid)))
        term (goal-args->tuple arglist)
        ]
    (if (empty? goal-specs)
      env
      (reduce #(apply-valid-post-spec %1 term %2) env goal-specs))))

(defmulti evaluate-goal-relationships (fn [env goal data] (:goal goal)))

(defmethod evaluate-goal-relationships :default [env goal data]
  env)

(defn evaluate-goal [data env goal]
  (-> env
      (evaluate-goal-pre-specs goal data)
      (evaluate-goal-post-specs goal data)
      (evaluate-goal-relationships goal data)))

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

(defn analyzing [data {arglist :arglist body :body :as clause} pre-spec]
  (-> (initial-env data arglist pre-spec)
      (evaluate-body data body)
      add-relationships
      rel/fixpoint-analysis
      ))

(defn complete-analysis [printer data]
  (log/debug "Start analysis of the clause")
  (doseq [pred-id (utils/get-pred-identities data)
          clause-number (utils/get-clause-identities-of-pred pred-id data)]
    (let [pre-spec (do
                     (log/debug (str pred-id))
                     (r/simplify-or
                      (->> (utils/get-specs-of-pred pred-id data)
                           :pre-specs
                           (map replace-specvars-with-uuid)
                           (map r/->TupleSpec)
                           r/->OneOfSpec)
                      (:specs data)))]
      ;(log/debug (str "Clause: " [clause-number (r/to-string pre-spec)]))
      (printer (str pred-id " " clause-number " ## " (r/to-string pre-spec)) (analyzing data (utils/get-clause pred-id clause-number data) pre-spec))
      )))
