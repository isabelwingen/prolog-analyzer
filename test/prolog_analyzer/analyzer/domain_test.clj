(ns prolog-analyzer.analyzer.domain-test
  (:require [prolog-analyzer.analyzer.domain :as sut]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [prolog-analyzer.utils :as utils]
            [ubergraph.core :as uber]
            [clojure.test :refer [deftest are is]]
            [clojure.template :refer [do-template]]))


(deftest add-to-dom
  (do-template [m]
      (let [{term :term spec :spec result :result} m]
        (is (= result (utils/env->map (sut/add-to-dom (uber/digraph) term spec)))))

    {:term     (ru/to-head-tail-list (r/->VarTerm "A") (r/->VarTerm "B"))
     :spec     (r/->TupleSpec [(r/->IntegerSpec) (r/->AtomSpec)])
     :result   {"[A, B]"    ["List(Any)" "Tuple(Integer, Atom)"]
                "[B]"       ["List(Any)" "Tuple(Atom)"]
                "A"         ["Any" "Integer"]
                "B"         ["Any" "Atom"]
                "[]"        ["EmptyList" "Tuple()"]
                ["A" "[A, B]"]    :is-head
                ["[B]" "[A, B]"]  :is-tail
                ["B" "[B]"]       :is-head
                ["[]" "[B]"]      :is-tail

                }}

    {:term (r/->EmptyListTerm)
     :spec (r/->TupleSpec [])
     :result {"[]" ["EmptyList" "Tuple()"]}}

    ;;Does not throw an error at the moment, as we only collect
    {:term (r/->EmptyListTerm)
     :spec (r/->TupleSpec [(r/->IntegerSpec)])
     :result {"[]" ["EmptyList" "Tuple(Integer)"]}}

    ))
