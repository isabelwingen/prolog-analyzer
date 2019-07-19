(ns prolog-analyzer.analyzer.core
  (:require [prolog-analyzer.utils :as utils]
            [prolog-analyzer.record-utils :as i]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.analyzer.env-for-header :as for-header]
   ))




(defn- analyze-clause [data clause pre-spec]
  (for-header/get-env data clause pre-spec))

(defn- get-pre-spec [pred-id data]
  (i/simplify
   (->> (utils/get-specs-of-pred pred-id data)
        :pre-specs
                                        ;(map replace-specvars-with-uuid)
        (map r/->TupleSpec)
        set
        r/->OneOfSpec)
   (:specs data)))

(defn complete-analysis [data]
  (when (empty? (utils/get-pred-identities data))
    (println (pr-str "No predicates found")))
  (for [pred-id (utils/get-pred-identities data)
        clause-number (utils/get-clause-identities-of-pred pred-id data)]
    (analyze-clause
     data
     (utils/get-clause pred-id clause-number data)
     (get-pre-spec pred-id data))))
