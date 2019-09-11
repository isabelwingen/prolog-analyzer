(ns prolog-analyzer.parser.create-pre-specs
  (:require [prolog-analyzer.utils :as utils :refer [case+]]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [clojure.tools.logging :as log]
            [prolog-analyzer.state :as state]
            [clojure.spec.alpha :as s]
            [prolog-analyzer.specs :as specs]
            [orchestra.spec.test :as stest]
            [orchestra.core :refer [defn-spec]]
            )
  )

(def tmp-data (atom {}))

(defn-spec simple-term boolean?
  [term ::specs/term]
  (not (#{r/COMPOUND, r/EXACT, r/LIST} (r/term-type term))))

(defn-spec vec-remove vector?
  {:fn #(s/and (= (-> % :args :coll count) (-> % :ret count))
               (< (-> % :args :pos) (-> % :args :coll count))
               (>= (-> % :args :pos) 0))}
  [coll ::specs/not-empty-arglist, pos integer?]
  (->> (vec (concat (subvec coll 0 pos) (subvec coll (inc pos))))
       (map #(if (simple-term %) (r/term-type %) %))
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
  (ru/simplify (r/->OneOfSpec (set (map r/initial-spec specs)))))

(defn-spec to-maybe-spec ::specs/spec
  [spec ::specs/spec]
  (case+ (r/spec-type spec)
         (r/VAR, r/ANY) spec
         r/OR (update spec :arglist conj (r/->VarSpec))
         (r/->OneOfSpec (hash-set spec (r/->VarSpec)))))

(defn-spec make-vec (s/coll-of ::specs/spec :min-count 0)
  {:fn #(= (-> % :args :arity) (-> % :ret count))}
  [arity pos?, coll sequential?]
  (vec coll))

(defn-spec pack-together (s/coll-of ::specs/spec :min-count 1)
  [arity pos-int?, group-of-specs (s/coll-of ::specs/not-empty-arglist :min-count 1)]
  (->> group-of-specs
       (apply map hash-set)
       (map to-spec)
       (map to-maybe-spec)
       (make-vec arity)
       ))

(def process (atom 0))

(s/fdef create-pre-specs
  :args (s/cat :pred-id ::specs/pred-id :clauses coll?)
  :ret ::specs/pre-specs)

(defmulti create-pre-specs (fn [[_ _ arity :as pred-id] clauses]
                             (cond
                               (= arity 0) :zero
                               (= arity 1) :one
                               (> arity 5) :too-big
                               :else :ok)))

(defmethod create-pre-specs :zero
  [pred-id clauses]
  [])

(defmethod create-pre-specs :one
  [pred-id clauses]
  (->> clauses
       (map :arglist)
       (map first)
       to-spec
       to-maybe-spec
       vector
       vector
       ))


(defmethod create-pre-specs :too-big
  [[_ _ arity] _]
  [(vec (repeat arity (r/->AnySpec)))]
  )


(defmethod create-pre-specs :ok
  [[_ goal-name _ :as pred-id] clauses]
  (if (#{:if :or} goal-name)
    []
    (->> clauses
         (map :arglist)
         find-best-grouping
         (map (partial pack-together (last pred-id)))
         vec)))

(s/fdef should-pre-spec-be-added?
  :args (s/cat :pred-id ::specs/pred-id :data ::specs/data)
  :ret boolean?)

(defn- should-pre-spec-be-added? [[_ goal arity :as pred-id] data]
  (let [res (and
             ;(nil? (utils/get-pre-specs pred-id @tmp-data))
             (> arity 0)
             (not= goal :if)
             (not= goal :or))]
    (log/debug "should be added? " (str pred-id) " " res)
    res))

(defn- add-any-pre-spec [pred-id clauses]
  (swap! process inc)
  (log/debug "Add Pre Spec - " (str pred-id) " - start - " @process)
  (swap! tmp-data assoc-in [:pre-specs pred-id] (create-pre-specs pred-id clauses))
  (log/debug "Add Pre Spec - " (str pred-id) " - end"))

(defn finish []
  (let [res @tmp-data]
    (log/debug "Add Pre Specs - done")
    res))

(defn bla [data]
  (->> data
       utils/get-pred-identities))


(defn add-any-pre-specs
  [data]
  (log/debug "Add Pre Specs")
  (reset! tmp-data data)
  (reset! process 0)
  (let [tasks (for [pred-id (->> data
                                 utils/get-pred-identities)
                      :let [clauses (utils/get-clauses-of-pred pred-id data)]]
                  [pred-id clauses])]
    (doall (pmap (partial apply add-any-pre-spec) tasks))
    @tmp-data
    ))
