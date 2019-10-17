(ns prolog-analyzer.analyzer.post-specs
  (:require [ubergraph.core :as uber]
            [prolog-analyzer.utils :as utils]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [clojure.spec.alpha :as s]
            [prolog-analyzer.specs :as specs]
            [orchestra.spec.test :as stest]
            [orchestra.core :refer [defn-spec]]
            ))


(defn-spec ^:private replace-id-with-arg ::specs/resolved-typing
  [arglist ::specs/arglist, {id :id :as p} ::specs/typing]
  (-> p
      (assoc :arg (nth arglist id))
      (dissoc :id)))

(defn-spec ^:private replace-ids-with-args ::specs/resolved-post-spec
  [postspec ::specs/post-spec, arglist ::specs/arglist]
  (-> postspec
      (update :guard (partial map (partial replace-id-with-arg arglist)))
      (update :conclusion (partial map (partial map (partial replace-id-with-arg arglist))))
      ))

(defn-spec ^:private register-post-spec ::specs/env
  [env ::specs/env, arglist ::specs/arglist, post-spec ::specs/post-spec]
  (if (uber/has-node? env :environment)
    (utils/update-attr env :environment :post-specs conj (replace-ids-with-args post-spec arglist))
    (-> env
        (uber/add-nodes :environment)
        (uber/add-attr :environment :post-specs [])
        (register-post-spec arglist post-spec))))

(defn-spec register-post-specs ::specs/env
  "Adds a postspec as active to an environment"
  [env ::specs/env, arglist ::specs/arglist, post-specs ::specs/post-specs]
  (reduce #(register-post-spec %1 arglist %2) env post-specs))

(defn-spec ^:private get-post-specs ::specs/resolved-post-specs
  [env ::specs/env]
  (if (uber/has-node? env :environment)
    (or (uber/attr env :environment :post-specs) [])
    []))

(s/def ::alias-map (s/map-of string? ::specs/spec))
(s/def ::alias-map-or-nil (s/or :map ::alias-map :nil nil?))

(defn-spec ^:private match-guard-with-type ::alias-map-or-nil
  [guard-type ::specs/spec actual-type ::specs/spec]
  (let [merge-fn #(if (empty? %)
                    (r/->ErrorSpec "Empty alias!")
                    (ru/simplify (r/->OneOfSpec %) false))
        placeholders (ru/find-placeholders (ru/intersect guard-type actual-type false))
        map (->> placeholders
                 (group-by :name)
                 (mapcat (fn [[k v]] [k (map #(or (:alias %) (r/->AnySpec)) v)]))
                 (apply hash-map)
                 (reduce-kv #(assoc %1 %2 (merge-fn %3)) {}))]
    (if (or
         (empty? placeholders)
         (some ru/error-spec? (flatten (vals map))))
      nil
      map)))

(s/def ::arg ::specs/term)
(s/def ::type ::specs/spec)

(defn-spec is-guard-true? ::alias-map-or-nil
  [env ::specs/env {arg :arg guard-type :type :as p} (s/keys :req-un [::arg ::type])]
  (let [actual-type (utils/get-dom-of-term env arg)]
    (if (ru/contains-placeholder? guard-type)
      (match-guard-with-type guard-type actual-type)
      (if (ru/same? actual-type (ru/intersect actual-type guard-type false))
        {}
        nil))))

(defn-spec ^:private merge-single-guard-values ::alias-map-or-nil
  [guard-maps (s/coll-of ::alias-map-or-nil)]
  (if (some nil? guard-maps)
    nil
    (let [to-seq (fn [x] (if (seq? x) x [x]))
          res (->> guard-maps
                   (apply merge-with (comp flatten vector))
                   (reduce-kv #(assoc %1 %2 (ru/simplify (r/->AndSpec (to-seq %3)) false)) {}))]
      (if (some ru/error-spec? (vals res))
        nil
        res))))

(defn-spec ^:private post-spec-applicable? ::alias-map-or-nil
  [env ::specs/env {guards :guard} ::specs/resolved-post-spec]
  (->> guards
       (map (partial is-guard-true? env))
       merge-single-guard-values))

(defn-spec ^:private complete-conclusion (s/map-of ::specs/term ::specs/spec)
  [part ::specs/resolved-typings arglist ::specs/arglist]
  (let [specifics-as-map (reduce #(assoc %1 (:arg %2) (:type %2)) {} part)
        all-as-map (reduce #(assoc %1 %2 (r/->AnySpec)) {} arglist)]
    (merge all-as-map specifics-as-map)))

(defn-spec ^:private create-step-from-post-spec ::specs/step
  [{conclusions :conclusion} ::specs/resolved-post-spec alias-map ::alias-map]
  (let [args (set (mapcat #(map :arg %) conclusions))
        new-conc (map #(complete-conclusion % args) conclusions)
        spec (->> new-conc
                  (map (fn [conc] (map conc args)))
                  (map r/->TupleSpec)
                  set
                  r/->OneOfSpec
                  ru/simplify
                  (ru/replace-placeholder-with-alias alias-map))
        tuple (apply ru/to-head-tail-list args)]
    [tuple spec]))


(defn-spec ^:private create-step (s/or :nil nil? :step ::specs/step)
  [env ::specs/env post-spec ::specs/resolved-post-spec]
  (if-let [alias-map (post-spec-applicable? env post-spec)]
    (create-step-from-post-spec post-spec alias-map)
    nil))

(defn-spec get-next-steps-from-post-specs ::specs/steps
  "Calculates steps from the registered, applicable postspecs"
  [env ::specs/env]
  (let [post-specs (get-post-specs env)]
    (->> post-specs
         (map (partial create-step env))
         (remove nil?)
         set)))

(stest/instrument)
