(ns prolog-analyzer.analyzer.core
  (:require
   [prolog-analyzer.parser :refer [process-prolog-file process-prolog-snippets process-prolog-files]] ;; used only during development
   [prolog-analyzer.analyzer.domain :as dom]
   [prolog-analyzer.records :as r]
   [prolog-analyzer.utils :as utils]
   [prolog-analyzer.analyzer.pretty-printer :as my-pp]
   [ubergraph.core :as uber]
   [ubergraph.protocols]
   [loom.graph]
   [loom.attr]
   [clojure.set]
   [clojure.pprint :as pp]
   [clojure.tools.namespace.repl :refer [refresh]]
   ))

(def data (atom {}))
(def last-uuid (atom 0))

(defn get-next-uuid []
  (inc @last-uuid))


(defn replace-specvars-with-uuid
  ([pre-spec]
   (let [specvars (->> pre-spec
                       (reduce #(concat %1 (r/find-specvars %2)) [])
                       distinct
                       (map :name))
         ids (take (count specvars) (drop (get-next-uuid) (range)))
         uuid-map (apply hash-map (interleave specvars ids))]
     (if ((complement empty?) ids)
       (reset! last-uuid (last ids)))
     (vector (map #(reduce-kv r/replace-specvar-name-with-value % uuid-map) pre-spec))))
  ([pre-spec & pre-specs] (reduce #(concat %1 (replace-specvars-with-uuid %2)) [] (cons pre-spec pre-specs))))

(defmulti add-relationships-aux (comp type second))
(defmethod add-relationships-aux prolog_analyzer.records.ListTerm [[env {head :head tail :tail :as term}]]
  (if (r/empty-list? tail)
    (-> env
        (dom/fill-env-for-term-with-spec head (r/make-spec:any))
        (uber/add-edges [head term {:relation :is-head}]))
    (-> env
        (dom/fill-env-for-term-with-spec head (r/make-spec:any))
        (dom/fill-env-for-term-with-spec tail (r/make-spec:list (r/make-spec:any)))
        (uber/add-edges [head term {:relation :is-head}] [tail term {:relation :is-tail}]))))

(defmethod add-relationships-aux prolog_analyzer.records.CompoundTerm [[env {functor :functor arglist :arglist :as term}]]
  (apply
   uber/add-edges
   (dom/multiple-fills env arglist (repeat (count arglist) (r/make-spec:any)))
   (map-indexed #(vector %2 term {:relation :arg-at-pos :pos %1}) arglist)))


(defmethod add-relationships-aux :default [[env _]]
  env)

(defn add-relationships [env]
  (reduce #(add-relationships-aux [%1 %2]) env (utils/get-terms env)))


(defn evaluate-goal [env {goal-name :goal module :module arity :arity arglist :arglist :as goal}]
  (let [goal-specs (some->> @data
                            (utils/get-specs-of-pred [module goal-name arity])
                            (:pre-specs)
                            (apply replace-specvars-with-uuid))

        [term goal-specs-as-tuple] [(if (= arity 1) (first arglist) (apply r/to-head-tail-list arglist))
                                    (if (= arity 1) (map first goal-specs) (map (partial apply r/to-tuple-spec) goal-specs))]]
    (if (and (> arity 0) goal-specs)
      (if (= 1 (count goal-specs))
        (dom/fill-env-for-term-with-spec env term (first goal-specs-as-tuple))
        (dom/fill-env-for-term-with-spec env term (apply r/to-or-spec goal-specs-as-tuple)))
      env)))

(defn evaluate-body [env body]
  (reduce evaluate-goal env body))

(defn- add-index-to-input-arguments [env arglist]
  (apply uber/add-nodes-with-attrs env (map-indexed #(vector %2 {:index %1}) arglist)))

(defn initial-env [arglist pre-spec]
  (-> (uber/digraph)
      (uber/add-nodes-with-attrs [:ENVIRONMENT {:user-defined-specs (get @data :specs)}])
      (dom/multiple-fills true arglist pre-spec)
      (add-index-to-input-arguments arglist)))

(defn analyzing [{arglist :arglist body :body :as clause} pre-spec]
  (-> (initial-env arglist pre-spec)
      (evaluate-body body)
      (add-relationships)
      ))

(defn complete-analysis [input-data]
  (reset! data input-data)
  (reset! last-uuid 0)
  (for [pred-id (utils/get-pred-identities @data)
        clause-id (utils/get-clause-identities-of-pred pred-id @data)
        pre-spec (:pre-specs (utils/get-specs-of-pred pred-id @data))]
    (let [mod-pre-spec (first (replace-specvars-with-uuid pre-spec))]
      [[clause-id mod-pre-spec] (analyzing (utils/get-clause clause-id @data) mod-pre-spec)]
      )))

(defn playground []
  (->> "prolog/playground.pl"
       process-prolog-file
       complete-analysis
       my-pp/pretty-print-analysis-result
       ))

(defn example []
  (->> "resources/analysis.pl"
       process-prolog-file
       complete-analysis
       my-pp/pretty-print-analysis-result
       ))

(defn example2 []
  (->> ["resources/module1.pl" "resources/module2.pl"]
       (apply process-prolog-files)
       complete-analysis
       my-pp/pretty-print-analysis-result
       ))
