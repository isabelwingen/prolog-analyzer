(ns prolog-analyzer.analyzer.global-analysis-test
  (:require [prolog-analyzer.analyzer.global-analysis :as sut]
            [clojure.test :refer [deftest is are]]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.parser :as parser]))


(defn get-data [] (parser/process-prolog-file "swipl" "prolog/prolog_analyzer.pl" "swipl" "resources/global2.pl"))

(defn execute-step [data]
  (let [envs (sut/step data)
        new-data (sut/add-new-knowledge data envs)]
    new-data))

(deftest valid-spec
  (are [x] (sut/valid-spec? x)
    (r/->IntegerSpec)
    (r/->AtomSpec)
    (r/->TupleSpec [])
    (r/map->CompoundSpec {})
    (r/->CompoundSpec "foo" [])
    (r/->CompoundSpec nil [(r/->AnySpec)])
    (r/->TupleSpec [(r/->TupleSpec [(r/->AtomSpec)])])
    (r/->OneOfSpec #{(r/->AnySpec)})
    (r/->ListSpec (r/->AnySpec))
    (r/->AndSpec #{(r/->OneOfSpec #{(r/->ListSpec (r/->AnySpec))})}))
  (are [x] ((complement sut/valid-spec?) x)
    (r/->ListSpec nil)
    (r/->CompoundSpec "foo" nil)
    (r/->CompoundSpec "foo" [nil])
    (r/->CompoundSpec "foo" [(r/->AtomSpec) nil])
    (r/->TupleSpec [nil])
    (r/->OneOfSpec nil)
    (r/->OneOfSpec [])
    (r/->OneOfSpec #{})
    (r/->OneOfSpec [nil])
    (r/->AndSpec nil)
    (r/->AndSpec [])
    (r/->AndSpec #{})
    (r/->AndSpec #{nil})
    ))
