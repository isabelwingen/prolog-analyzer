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

;; CREATE PRE SPEC
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

(defn-spec create-pre-spec ::specs/pre-specs
  [pred-id ::specs/pred-id, data ::specs/data]
  (let [clauses (->> data
                     (utils/get-clauses-of-pred pred-id))]
    (->> clauses
         (map :arglist)
         find-best-grouping
         (map (partial pack-together (last pred-id)))
         vec)))

(s/fdef should-pre-spec-be-added?
  :args (s/cat :pred-id ::specs/pred-id :data map?)
  :ret boolean?)

(defn- should-pre-spec-be-added? [[_ goal arity :as pred-id] data]
  (and
   (nil? (utils/get-pre-specs pred-id data))
   (> arity 0)
   (not= goal :if)
   (not= goal :or)))


(defn add-any-pre-specs
  "If there are no pre-specs, add one"
  [data]
  (loop [pred-ids (utils/get-pred-identities data)
         result data]
    (if-let [[module pred-name arity :as pred-id] (first pred-ids)]
      (if (should-pre-spec-be-added? pred-id data)
        (recur (rest pred-ids) (assoc-in result [:pre-specs [module pred-name arity]] (create-pre-spec pred-id data)))
        (recur (rest pred-ids) result))
      result)))


(stest/instrument)
