(ns prolog-analyzer.analyzer.core
  (:require [prolog-analyzer.utils :as utils]
            [prolog-analyzer.intersect :as i]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.analyzer.env-for-header :as for-header]
   ))




(defn analyze-clause [data clause pre-spec]
  (for-header/get-env data clause pre-spec))

(defn complete-analysis [data]
  (when (empty? (utils/get-pred-identities data))
    (println (pr-str "No predicates found")))
  (for [pred-id (utils/get-pred-identities data)
        clause-number (utils/get-clause-identities-of-pred pred-id data)]
    (let [pre-spec (i/simplify
                    (->> (utils/get-specs-of-pred pred-id data)
                         :pre-specs
                         ;(map replace-specvars-with-uuid)
                         (map r/->TupleSpec)
                         set
                         r/->OneOfSpec)
                    (:specs data))]
      (analyze-clause data (utils/get-clause pred-id clause-number data) pre-spec pred-id clause-number))))

(defn complete-analysis-parallel [data]
  (when (empty? (utils/get-pred-identities data))
    (println (pr-str "No predicates found")))
  (let [tasks (for [pred-id (utils/get-pred-identities data)
                    clause-number (utils/get-clause-identities-of-pred pred-id data)]
                (let [pre-spec (i/simplify
                                (->> (utils/get-specs-of-pred pred-id data)
                                     :pre-specs
                                    ; (map replace-specvars-with-uuid)
                                     (map r/->TupleSpec)
                                     set
                                     r/->OneOfSpec)
                                (:specs data))]
                  {:clause (utils/get-clause pred-id clause-number data) :pre-spec pre-spec :pred-id pred-id :clause-number clause-number}
                  ))]
    (pmap (fn [{clause :clause pre-spec :pre-spec pred-id :pred-id clause-number :clause-number}] (analyze-clause clause pre-spec pred-id clause-number)) tasks)))
