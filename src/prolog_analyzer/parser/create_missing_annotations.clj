(ns prolog-analyzer.parser.create-missing-annotations
  (:require [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]
            [flatland.ordered.set :refer [ordered-set]]
            [orchestra.core :refer [defn-spec]]
            [prolog-analyzer.record-utils :as ru]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.specs :as specs]
            [prolog-analyzer.utils :as utils :refer [case+]]))

(def TRESHOLD 5)

(def tmp-data (atom {}))

(defn-spec simple-term boolean?
  [term ::specs/term]
  (not (#{r/COMPOUND, r/EXACT, r/LIST} (ru/term-type term))))

(defn-spec vec-remove vector?
  {:fn #(s/and (= (-> % :args :coll count) (-> % :ret count))
               (< (-> % :args :pos) (-> % :args :coll count))
               (>= (-> % :args :pos) 0))}
  [coll ::specs/not-empty-arglist, pos integer?]
  (->> (vec (concat (subvec coll 0 pos) (subvec coll (inc pos))))
       (map #(if (simple-term %) (ru/term-type %) %))
       vec))

(defn-spec find-best-grouping (s/coll-of ::specs/arglists)
  [arglists ::specs/arglists]
  (let [arity (count (first arglists))
        juxt-fns (map (fn [i] (partial group-by #(vec-remove % i))) (range 0 arity))
        groups ((apply juxt juxt-fns) arglists)
        best-group (first (sort #(< (count (keys %1)) (count (keys %2))) groups))]
    (vals best-group)))

(defn-spec to-spec ::specs/spec
  [specs ::specs/not-empty-arglist]
  (ru/simplify (r/->OneOfSpec (set (map ru/initial-spec specs)))))

(defn-spec to-maybe-spec ::specs/spec
  [spec ::specs/spec]
  (case+ (ru/spec-type spec)
         (r/VAR, r/ANY) spec
         r/OR (update spec :arglist conj (r/->VarSpec))
         (r/->OneOfSpec (hash-set spec (r/->VarSpec)))))

(defn-spec make-vec (s/coll-of ::specs/spec :min-count 0)
  {:fn #(= (-> % :args :arity) (-> % :ret count))}
  [arity pos?, coll sequential?]
  (vec coll))

(defn-spec pack-together (s/coll-of ::specs/spec :min-count 1)
  [arity pos-int?,
   to-maybe? boolean?,
   group-of-specs (s/coll-of ::specs/not-empty-arglist :min-count 1)]
  (let [modifier (if to-maybe? to-maybe-spec identity)]
    (->> group-of-specs
         (apply map hash-set)
         (map to-spec)
         (map modifier)
         (make-vec arity)
         )))

(def process (atom 0))

(s/fdef create-tuples
  :args (s/cat :pred-id ::specs/pred-id :clauses coll? :to-maybe? boolean?)
  :ret ::specs/pre-specs)

(defmulti create-tuples (fn [[_ _ arity :as pred-id] clauses to-maybe?]
                             (cond
                               (= arity 1) :one
                               (> arity TRESHOLD) :too-big
                               :else :ok)))

(defmethod create-tuples :one
  [pred-id clauses to-maybe?]
  (let [modifier (if to-maybe? to-maybe-spec identity)]
    (->> clauses
         (map :arglist)
         (map first)
         to-spec
         modifier
         vector
         vector
         )))

(defmethod create-tuples :too-big
  [[_ _ arity] _ _]
  [(vec (repeat arity (r/->AnySpec)))]
  )

(defmethod create-tuples :ok
  [[_ goal-name _ :as pred-id] clauses to-maybe?]
  (if (#{:if :or :not} goal-name)
    []
    (->> clauses
         (map :arglist)
         find-best-grouping
         (map (partial pack-together (last pred-id) to-maybe?))
         vec)))

(s/fdef should-pre-spec-be-added?
  :args (s/cat :pred-id ::specs/pred-id)
  :ret boolean?)

(defn- should-pre-spec-be-added? [[_ goal arity :as pred-id]]
  (let [res (and
             (nil? (utils/get-pre-specs pred-id @tmp-data))
             (> arity 0)
             (not= goal :if)
             (not= goal :not)
             (not= goal :or))]
    (log/trace "should be added? " (str pred-id) " " res)
    res))

(defn- should-post-spec-be-added [[_ goal arity :as pred-id]]
  (let [res (and
             (nil? (utils/get-post-specs pred-id @tmp-data))
             (> arity 0)
             (not= goal :if)
             (not= goal :not)
             (not= goal :or))]
    (log/trace "should be added? " (str pred-id) " " res)
    res))


(defn- add-pre-spec [pred-id clauses]
  (when (should-pre-spec-be-added? pred-id)
    (swap! process inc)
    (log/trace "Add Pre Spec - " (str pred-id) " - start - " @process)
    (swap! tmp-data assoc-in [:pre-specs pred-id] (create-tuples pred-id clauses true))
    (log/trace "Add Pre Spec - " (str pred-id) " - end")))

(defn- tuples-to-post-specs [tuples]
  (r/->Postspec [] (vec (map (comp vec (partial map-indexed #(hash-map :id %1 :type %2))) tuples))))

(defn- add-post-spec [pred-id clauses]
  (when (should-post-spec-be-added pred-id)
    (swap! tmp-data assoc-in [:post-specs pred-id] (ordered-set (tuples-to-post-specs (create-tuples pred-id clauses false))))))

(defn- add-missing-annotations [pred-id clauses]
  (add-pre-spec pred-id clauses)
  (add-post-spec pred-id clauses))

(defn- finish []
  (let [res @tmp-data]
    (log/debug "Add Pre Specs - done")
    (reset! tmp-data {})
    res))

(defn start [data]
  (log/trace "Add Pre Specs")
  (reset! tmp-data data)
  (reset! process 0)
  (let [tasks (for [pred-id (->> data
                                 utils/get-pred-identities)
                      :let [clauses (utils/get-clauses-of-pred pred-id data)]]
                  [pred-id clauses])]
    (doall (pmap (partial apply add-missing-annotations) tasks))
    (finish)
    ))
