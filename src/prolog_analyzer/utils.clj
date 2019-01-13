(ns prolog-analyzer.utils
  (:require [prolog-analyzer.analyzer.built-in-specs :as built-ins]
            [ubergraph.core :as uber]
            [ubergraph.protocols]
            [loom.graph]
            [loom.attr]
            ))

;; for data extracted from a prolog file


(defn get-specs-of-pred [[module pred-name arity :as pred-identity] data]
  (if (= :built-in module)
    (built-ins/get-specs-of-built-in-pred pred-name arity)
    (-> data
        (select-keys [:pre-specs :post-specs :inv-specs])
        (update :pre-specs #(get-in % pred-identity))
        (update :post-specs #(get-in % pred-identity))
        (update :inv-specs #(get-in % pred-identity))
        )))

(defn get-pred-identities [data]
  (for [module (keys (:preds data))
        pred-name (keys (get-in data [:preds module]))
        arity (keys (get-in data [:preds module pred-name]))]
    [module pred-name arity]))

(defn get-clause-identities [data]
  (let [preds (:preds data)]
    (for [pred-id (get-pred-identities data)
          clause-number (keys (get-in preds pred-id))]
      (conj pred-id clause-number))))

(defn get-clause-identities-of-pred [pred-id data]
  (for [clause-number (keys (get-in (:preds data) pred-id))]
    (conj pred-id clause-number)))

(defn get-clause [clause-id data]
  (get-in (:preds data) clause-id))

(defn get-clauses-of-pred [pred-identity data]
  (vals (get-in data (apply vector :preds pred-identity))))

(defn empty-list? [{term :term type :type}]
  (and (= type :atomic) (= term "[]")))


;; transform head-tail-list
(defn to-head-tail-list [& specs]
  (if (empty? specs)
    {:term "[]" :type :atomic}
    {:type :list :head (first specs) :tail (apply to-head-tail-list (rest specs))}))

(to-head-tail-list :a :b :c)

(defn head-tail-list-to-list [{head :head tail :tail}]
  (if (= "[]" (:term tail))
    {:type :list :elements [head]}
    (let [{rest-args :elements} (head-tail-list-to-list tail)]
      {:type :list :elements (apply vector head rest-args)})))

(defn get-elements-of-list [{head :head tail :tail}]
  (if (empty-list? tail)
    (list head)
    (conj (get-elements-of-list tail) head)))

;; graphs
(defn transitive-closure [g]
  (let [tmp-graph (atom g)]
    (doseq [k (uber/nodes @tmp-graph)
            i (uber/nodes @tmp-graph)
            :when (uber/find-edge @tmp-graph i k)]
      (doseq [j (uber/nodes @tmp-graph)
              :when (uber/find-edge @tmp-graph k j)]
        (swap! tmp-graph uber/add-edges [i j])))
    @tmp-graph))

(defn transitive-closure-with-attr [g attr-map]
  (let [tmp-graph (atom g)]
    (doseq [k (uber/nodes @tmp-graph)
            i (uber/nodes @tmp-graph)
            :when (uber/find-edge @tmp-graph (-> attr-map (assoc :src i) (assoc :dest k)))]
      (doseq [j (uber/nodes @tmp-graph)
              :when (uber/find-edge @tmp-graph k j)]
        (swap! tmp-graph uber/add-edges [i j])))
    @tmp-graph))

(defn- merge-attr [attr1 attr2]
  (let [dom1 (:dom attr1)
        dom2 (:dom attr2)]
    (assoc (merge attr1 attr2)
           :dom
           (apply vector (concat dom1 dom2)))))



(defn- merge-nodes [env1 env2]
  (loop [result env1
         nodes (uber/nodes env2)]
    (if-let [node (first nodes)]
      (if (uber/has-node? result node)
        (recur (uber/set-attrs result node (merge-attr (uber/attrs result node) (uber/attrs env2 node))) (rest nodes))
        (recur (uber/add-nodes-with-attrs result [node (uber/attrs env2 node)]) (rest nodes)))
      result)))

(defn- merge-edges [env1 env2]
  (apply uber/add-edges env1 (uber/edges env2)))


(defn merge-envs [env & envs]
  (reduce
   merge-edges
   (reduce merge-nodes env envs)
   envs))

(defn dom-map-to-env [dom-map]
  (let [nodes (map #(vector % {:dom (get dom-map %)}) (keys dom-map))]
    (apply uber/add-nodes-with-attrs (uber/digraph) nodes)))
