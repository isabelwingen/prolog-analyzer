(ns prolog-analyzer.utils
  (:require [ubergraph.core :as uber]
            [ubergraph.protocols]
            [loom.graph]
            ))

;; for data extracted from a prolog file
(defn get-specs-of-pred [pred-identity data]
  (let [spec-identity (rest pred-identity)
        specs data]
    (-> specs
        (select-keys [:pre-specs :post-specs :inv-specs])
        (update :pre-specs #(get-in % spec-identity))
        (update :post-specs #(get-in % spec-identity))
        (update :inv-specs #(get-in % spec-identity))
        )))

(defn get-impls-of-pred [pred-identity data]
  (vals (get-in data (apply vector :preds pred-identity))))

(defn get-pred-identities [data]
  (for [module (keys (:preds data))
        pred-name (keys (get-in data [:preds module]))
        arity (keys (get-in data [:preds module pred-name]))]
    [module pred-name arity]))

(defn get-clause-identities [data]
  (let [preds (:preds data)]
    (for [pred-id (get-pred-identities data)
          clause (keys (get-in preds pred-id))]
      (conj pred-id clause))))

(defn empty-list? [{term :term type :type}]
  (and (= type :atomic) (= term "[]")))


;; transform head-tail-list
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
