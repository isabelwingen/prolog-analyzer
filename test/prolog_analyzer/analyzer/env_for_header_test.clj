(ns prolog-analyzer.analyzer.env-for-header-test
  (:require [prolog-analyzer.analyzer.env-for-header :as sut]
            [prolog-analyzer.test-helper :as th]
            [prolog-analyzer.utils :as utils]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [clojure.test :refer [deftest are]]))


(defn simple-example []
  (let [data (th/read-in-file "resources/simple-example.pl" )
        pred-identity ["simple_example" "mmember" 2]
        first-clause (utils/get-clause pred-identity 0 data)
        second-clause (utils/get-clause pred-identity 1 data)
        pre-spec (ru/simplify
                  (->> (utils/get-specs-of-pred pred-identity data)
                       :pre-specs
                       (map r/->TupleSpec)
                       set
                       r/->OneOfSpec)
                  (:specs data))]
    (utils/env->map (sut/get-env data first-clause pre-spec))))



(defn test2 []
  (let [data {}
        clause {:arglist [(r/->VarTerm "H") (r/->ListTerm (r/->VarTerm "H") (r/->VarTerm "T"))]}
        spec (r/->TupleSpec [(r/->IntegerSpec) (r/->OneOfSpec #{(r/->ListSpec (r/->OneOfSpec #{(r/->IntegerSpec) (r/->AtomSpec)})) (r/->ListSpec (r/->FloatSpec))})])]
    (utils/env->map (sut/get-env data clause spec))))

(test2)
(simple-example)
