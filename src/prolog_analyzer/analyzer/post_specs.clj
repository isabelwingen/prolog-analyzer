(ns prolog-analyzer.analyzer.post-specs
  (:require [ubergraph.core :as uber]
            [prolog-analyzer.utils :as utils]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]))

(defn- replace-id-with-arg [arglist {id :id :as p}]
  (-> p
      (assoc :arg (nth arglist id))
      (dissoc :id)))

(defn- replace-ids-with-args [postspec arglist]
  (-> postspec
      (update :guard (partial map (partial replace-id-with-arg arglist)))
      (update :guard (partial apply vector))
      (update :conclusion (partial map (partial replace-id-with-arg arglist)))
      (update :conclusion (partial apply vector))
      ))

(defn- register-post-spec [env arglist {guard :guard conclusion :conclusion :as post-spec}]
  (if (uber/has-node? env :environment)
    (utils/update-attr env :environment :post-specs conj (replace-ids-with-args post-spec arglist))
    (-> env
        (uber/add-nodes :environment)
        (uber/add-attr :environment :post-specs [])
        (register-post-spec arglist post-spec))))

(defn register-post-specs [env arglist post-specs]
  (reduce #(register-post-spec %1 arglist %2) env post-specs))

(defn- get-post-specs [env]
  (if (uber/has-node? env :environment)
    (uber/attr env :environment :post-specs)
    []))


(defn- match-guard-with-type [defs guard-type actual-type exact?]
  (let [merge-fn (if exact?
                   #(ru/simplify (r/->AndSpec %) defs false)
                   #(ru/simplify (r/->OneOfSpec %) defs false))
        placeholders (ru/find-placeholders (ru/intersect guard-type actual-type defs false))
        map (->> placeholders
                 (group-by :name)
                 (mapcat (fn [[k v]] [k (map :alias v)]))
                 (apply hash-map)
                 (reduce-kv #(assoc %1 %2 (merge-fn %3)) {}))]
    (if (or
         (empty? placeholders)
         (some ru/error-spec? (flatten (vals map))))
      nil
      map)))

(defn- is-guard-true? [defs env {arg :arg type :type exact? :exact}]
  (if (ru/contains-placeholder? type)
    (match-guard-with-type defs type (utils/get-dom-of-term env arg) exact?)
    (if (ru/same? type (utils/get-dom-of-term env arg))
      {}
      nil)))

(defn- merge-single-guard-values [defs guard-maps]
  (let [res (->> guard-maps
                 (apply merge-with (comp flatten vector))
                 (reduce-kv #(assoc %1 %2 (if (coll? %3) (apply vector %3) [%3])) {})
                 (reduce-kv #(assoc %1 %2 (ru/simplify (r/->AndSpec %3) defs false)))
                 )]
    (if (some ru/error-spec? (vals res))
      nil
      res)))


(defn- is-post-spec-applicable? [defs env {guards :guard}]
  (->> guards
       (map (partial is-guard-true? defs env))
       (merge-single-guard-values defs)))


(defn- create-steps-from-post-spec [{conclusions :conclusion}]
  (map (juxt :arg :type) conclusions))

(defn- create-steps-from-post-specs [& post-specs]
  (mapcat create-steps-from-post-spec post-specs))

(defn apply-post-specs [env]
  (->> env
       get-post-specs
       (filter (partial is-post-spec-applicable? env))
       (apply create-steps-from-post-specs)))
