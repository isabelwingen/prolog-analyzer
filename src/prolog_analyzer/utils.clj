(ns prolog-analyzer.utils)

;; for data extracted from a prolog file
;; utils
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

