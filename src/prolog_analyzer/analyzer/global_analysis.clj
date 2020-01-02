(ns prolog-analyzer.analyzer.global-analysis
  (:require [clojure.tools.logging :as log]
            [flatland.ordered.set :refer [ordered-set]]
            [prolog-analyzer.analyzer.core :as clause-analysis]
            [prolog-analyzer.record-utils :as ru]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.state :as state]
            [prolog-analyzer.utils :as utils]
            [prolog-analyzer.result-visualizer :refer [print-type-information]]))

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
       set
       vec
       (r/->Postspec [])))

(defn- length-of-post-spec [{guard :guard concl :conclusion}]
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

(defn- faulty-postspec? [{guard :guard concs :conclusion}]
  (->> concs
       (apply concat guard)
       (map :type)
       (some ru/error-spec?)))


(defn- add-if-new-and-correct [data [_ _ arity :as pred-id] post-spec]
  (if (faulty-postspec? post-spec)
    data
    (if (> (length-of-post-spec post-spec) 1000)
      data
      (-> data
          (update-in [:post-specs pred-id] #(if (nil? %)
                                              (ordered-set)
                                              (apply ordered-set %)))
          (update-in [:post-specs pred-id] #(conj % post-spec))))))

;;TODO: remove group-envs. Iterate over envs, save created conclusion under pred-identities
;; when done, merge together to postspec
(defn- create-new-post-specs [in-data envs]
  (->> envs
       group-envs-by-pred-id
       (reduce-kv #(assoc %1 %2 (create-post-spec %3)) {})
       (reduce-kv add-if-new-and-correct in-data)))

(defn- add-errors [in-data envs]
  (->> envs
       (map utils/errors)
       (apply merge-with merge in-data)))

(defn- create-new-data [in-data envs]
  (-> in-data
      (create-new-post-specs envs)
      (add-errors envs)))

(defn- same [data-a data-b]
  (and
   (= (:post-specs data-a) (:post-specs data-b))
   (= (:pre-specs data-a) (:pre-specs data-b))
   (= (:errors data-a) (:errors data-b))))

(defn fixpoint
  "Calculates environments for every clause
  add adds the newly gained knwledge back to the data.

  Repeat until a fxpoint is reached"
  [write counter in-data]
  (log/info "Fixpoint: Step " counter)
  (write in-data counter)
  (let [envs (clause-analysis/complete-analysis in-data)
        new-data (create-new-data in-data envs)]
    (print-type-information (inc counter) envs)
    (if (same in-data new-data)
      (do
        (log/info "Done")
        (print-type-information 999 envs)
        new-data)
      (recur write (inc counter) new-data))))

(defn global-analysis
  "Starts the two-phased analysis."
  [write data]
  (log-if-empty data)
  (fixpoint write 0 data))
