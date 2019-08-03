(ns prolog-analyzer.analyzer.post-specs
  (:require [ubergraph.core :as uber]
            [prolog-analyzer.utils :as utils]
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

(defn- is-guard-true? [env {arg :arg type :type}]
  (= type (utils/get-dom-of-term env arg)))

(defn- is-post-spec-applicable? [env {guards :guard}]
  (every? (partial is-guard-true? env) guards))

(defn- create-steps-from-post-spec [{conclusions :conclusion}]
  (map (juxt :arg :type) conclusions))

(defn- create-steps-from-post-specs [& post-specs]
  (mapcat create-steps-from-post-spec post-specs))

(defn apply-post-specs [env]
  (->> env
       get-post-specs
       (filter (partial is-post-spec-applicable? env))
       (apply create-steps-from-post-specs)))
