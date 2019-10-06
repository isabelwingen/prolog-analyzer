(ns prolog-analyzer.analyzer.global-analysis
  (:require [prolog-analyzer.state :as state]
            [prolog-analyzer.utils :as utils :refer [case+]]
            [prolog-analyzer.record-utils :as ru]
            [prolog-analyzer.records :as r]
            [clojure.tools.logging :as log]
            [prolog-analyzer.result-visualizer :refer [print-intermediate-result]]
            [flatland.ordered.set :refer [ordered-set]]
            [prolog-analyzer.analyzer.core :as clause-analysis]
            [orchestra.core :refer [defn-spec]]
            [orchestra.spec.test :as stest]
            [clojure.spec.alpha :as s]
            [prolog-analyzer.specs :as specs]
            ))


(defn- log-if-empty [data]
  (when (empty? (utils/get-pred-identities data))
    (println (pr-str "No predicates found"))))

(defn- group-envs-by-pred-id [envs]
  (group-by #(vec (drop-last (utils/get-title %))) envs))

(defn- create-single-conclusion [env]
  (->> env
       utils/get-arguments
       (map-indexed (fn [i elem] {:id i :type (ru/simplify (utils/get-dom-of-term env elem))}))
       vec))

(defn- create-post-spec [envs]
  (->> envs
       (map create-single-conclusion)
       vec
       (r/->Postspec [])))

(defn length-of-post-spec [{guard :guard concl :conclusion}]
  (let [a (->> guard
               (map :type)
               (map r/length)
               (apply +))
        b (->> concl
               (map (partial map :type))
               (map (partial map r/length))
               (map (partial apply +))
               (apply +))]
    (+ a b)))



(defn is-weaker-spec? [a b]
  (= b (ru/intersect a b false)))

(defn add-missing-ids [max-id c]
  (let [ids (->> max-id
                 inc
                 (range 0)
                 (remove (set (map :id c)))
                 (map #(hash-map :id % :type (r/->AnySpec))))]
    (concat c ids)))

(defn transform-to-spec [{concs :conclusion}]
  (let [max-id (->> concs
                    flatten
                    (map :id)
                    (apply max))]
    (->> concs
         (map (partial add-missing-ids max-id))
         (map (partial sort-by :id))
         (map (partial map :type))
         (map (partial r/->TupleSpec))
         (map ru/simplify )
         set
         r/->OneOfSpec
         ru/simplify)))

(defn-spec is-weaker? boolean?
  [post-spec-a ::specs/post-spec,
   post-spec-b ::specs/post-spec]
  (let [a (transform-to-spec post-spec-a)
        b (transform-to-spec post-spec-b)]
    (is-weaker-spec? a b)))


(defn-spec simplify-post-specs ::specs/post-specs
  [l ::specs/post-specs]
  (loop [[x & y :as res] l
         counter 0]
    (if (= counter (count l))
      res
      (recur (conj (vec (remove #(is-weaker? % x) y)) x) (inc counter)))))

(defn-spec add-to-existing-post-spec ::specs/post-specs
  [all-post-specs ::specs/post-specs
   {guard :guard :as post-spec} ::specs/post-spec]
  (mapcat identity (->  (group-by :guard (map r/map->Postspec all-post-specs))
                        (update guard conj post-spec)
                        (update guard set)
                        (update guard simplify-post-specs)
                        vals)))

(defn-spec ordered-set? boolean? [l ::specs/post-specs]
  (= (apply ordered-set l) l))

(defn-spec add-to-post-specs (s/and ::specs/post-specs)
  [existing-post-specs ::specs/post-specs,
   post-spec ::specs/post-spec]
  (if (some #(is-weaker? post-spec %) existing-post-specs)
    existing-post-specs
    (->> existing-post-specs
         (remove #(is-weaker? % post-spec))
         (cons post-spec))))


(defn-spec add-if-new ::specs/data
  [data ::specs/data,
   pred-id ::specs/pred-id,
   post-spec ::specs/post-spec]
  (if
      (or
       (zero? (last pred-id))
       (> (length-of-post-spec post-spec) 1000))
    data
    (update-in data [:post-specs pred-id] add-to-post-specs post-spec)))


(defn-spec create-new-post-specs ::specs/data
  [in-data ::specs/data, envs ::specs/envs]
  (->> envs
       group-envs-by-pred-id
       (reduce-kv #(assoc %1 %2 (create-post-spec %3)) {})
       (reduce-kv add-if-new in-data)))

(defn-spec add-errors ::specs/data
  [in-data ::specs/data, envs ::specs/envs]
  (->> envs
       (map utils/errors)
       (apply merge-with merge in-data)))

(defn-spec create-new-data ::specs/data
  [in-data ::specs/data, envs ::specs/envs]
  (-> in-data
      (create-new-post-specs envs)
      (add-errors envs)))

(defn-spec same? boolean?
  [data-a ::specs/data, data-b ::specs/data]
  (and
   (= (:post-specs data-a) (:post-specs data-b))
   (= (:pre-specs data-a) (:pre-specs data-b))
   (= (:errors data-a) (:errors data-b))))


(defn-spec fixpoint ::specs/data
  [write fn?,
   counter (complement neg-int?),
   in-data ::specs/data]
  (log/info "Fixpoint: Step " counter)
  (write in-data counter)
  (let [envs (clause-analysis/complete-analysis in-data)
        new-data (create-new-data in-data envs)]
    (if (same? in-data new-data)
      (do
        (log/info "Done")
        new-data)
      (recur write (inc counter) new-data))))

(defn-spec dummy-post-spec ::specs/post-spec [arity pos-int?]
  (r/->Postspec [] [(vec (map #(hash-map :id % :type (r/->AnySpec)) (range 0 arity)))]))

(defn-spec add-dummy-post-specs ::specs/data [data ::specs/data]
  (reduce
   (fn [d [_ _ arity :as pred-id]]
     (if (> arity 0)
       (update-in d [:post-specs pred-id] #(->> arity
                                                dummy-post-spec
                                                (conj %)
                                                vec))
       d))
   data
   (utils/get-pred-identities data)))


(defn-spec global-analysis ::specs/data [write fn?, data ::specs/data]
  (let [cleared-data data #_(add-dummy-post-specs data)]
    (reset! state/self-calling {})
    (log-if-empty cleared-data)
    (fixpoint write 0 cleared-data)))

(stest/instrument)
