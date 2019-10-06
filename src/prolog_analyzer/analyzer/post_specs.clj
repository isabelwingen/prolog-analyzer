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

(defn- replace-id-with-arg [arglist {id :id :as p}]
  (-> p
      (assoc :arg (nth arglist id))
      (dissoc :id)))

(defn- replace-ids-with-args [postspec arglist]
  (-> postspec
      (update :guard (partial map (partial replace-id-with-arg arglist)))
      (update :conclusion (partial map (partial map (partial replace-id-with-arg arglist))))
      ))

(defn- register-post-spec [env arglist post-spec]
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

(s/fdef match-guard-with-type
  :args (s/cat :guard ::specs/spec :actual-type ::specs/spec)
  :ret (s/or :map map? :nil nil?))

(defn- match-guard-with-type [guard-type actual-type]
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
(s/fdef is-guard-true?
  :args (s/cat
         :env utils/is-graph?
         :p (s/keys :req-un [::arg ::type])))

(defn is-guard-true? [env {arg :arg guard-type :type :as p}]
  (let [actual-type (utils/get-dom-of-term env arg)]
    (if (ru/contains-placeholder? guard-type)
      (match-guard-with-type guard-type actual-type)
      (if (ru/same? actual-type (ru/intersect actual-type guard-type false))
        {}
        nil))))

(defn- merge-single-guard-values [guard-maps]
  (if (some nil? guard-maps)
    false
    (let [to-seq (fn [x] (if (seq? x) x [x]))
          res (->> guard-maps
                   (apply merge-with (comp flatten vector))
                   (reduce-kv #(assoc %1 %2 (ru/simplify (r/->AndSpec (to-seq %3)) false)) {}))]
      (if (some ru/error-spec? (vals res))
        nil
        res))))

(defn- post-spec-applicable? [env {guards :guard}]
  (->> guards
       (map (partial is-guard-true? env))
       merge-single-guard-values))

(defn- complete-conclusion [part arglist]
  (let [specifics-as-map (reduce #(assoc %1 (:arg %2) (:type %2)) {} part)
        all-as-map (reduce #(assoc %1 %2 (r/->AnySpec)) {} arglist)]
    (merge all-as-map specifics-as-map)))

(defn- create-step-from-post-spec
  [{conclusions :conclusion} alias-map]
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


(defn- create-step [env post-spec]
  (if-let [alias-map (post-spec-applicable? env post-spec)]
    (create-step-from-post-spec post-spec alias-map)
    nil))

(defn get-next-steps-from-post-specs [env]
  (let [post-specs (get-post-specs env)]
    (->> post-specs
         (map (partial create-step env))
         (remove nil?)
         set)))
