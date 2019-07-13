(ns prolog-analyzer.analyzer.domain-test
  (:require [prolog-analyzer.analyzer.domain :as sut]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [prolog-analyzer.utils :as utils]
            [ubergraph.core :as uber]
            [clojure.test :refer [deftest are is]]
            [clojure.template :refer [do-template]]))


(deftest next-step
  (are [term spec result]
      (= result (sut/next-steps term spec))
    (r/->ListTerm (r/->IntegerTerm 1) (r/->ListTerm (r/->IntegerTerm 2) (r/->EmptyListTerm)))
    (r/->TupleSpec [(r/->IntegerSpec) (r/->NumberSpec) (r/->IntegerSpec)])
    {:steps [[(r/->IntegerTerm 1) (r/->IntegerSpec)]
             [(r/->ListTerm (r/->IntegerTerm 2) (r/->EmptyListTerm)) (r/->TupleSpec [(r/->NumberSpec) (r/->IntegerSpec)])]]
     :edges []}))


(deftest add-to-dom
  (do-template [m]
      (let [{term :term spec :spec result :result} m]
        (is (= result (utils/env->map (sut/add-to-dom (uber/digraph) term spec)))))

    {:term     (ru/to-head-tail-list (r/->VarTerm "A") (r/->VarTerm "B"))
     :spec     (r/->TupleSpec [(r/->IntegerSpec) (r/->AtomSpec)])
     :result   {(ru/to-head-tail-list (r/->VarTerm "A") (r/->VarTerm "B"))     [(r/->ListSpec (r/->AnySpec)) (ru/tuple-with-anys 2)]
                (ru/to-head-tail-list (r/->VarTerm "B"))                       [(r/->ListSpec (r/->AnySpec))(ru/tuple-with-anys 1)]
                (r/->VarTerm "A")                                             [(r/->AnySpec) (r/->IntegerSpec)]
                (r/->VarTerm "B")                                             [(r/->AnySpec) (r/->AtomSpec)]
                (r/->EmptyListTerm)                                           [(r/->EmptyListSpec) (r/->TupleSpec [])]}}

    {:term (r/->EmptyListTerm)
     :spec (r/->TupleSpec [])
     :result {(r/->EmptyListTerm) [(r/->EmptyListSpec) (r/->TupleSpec [])]}}

    ;;Does not throw an error at the moment, as we only collect
    {:term (r/->EmptyListTerm)
     :spec (r/->TupleSpec [(r/->IntegerSpec)])
     :result {(r/->EmptyListTerm) [(r/->EmptyListSpec) (r/->TupleSpec [(r/->AnySpec)])]}}

    ))
