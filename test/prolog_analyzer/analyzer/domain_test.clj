(ns prolog-analyzer.analyzer.domain-test
  (:require [prolog-analyzer.analyzer.domain :as sut]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [prolog-analyzer.utils :as utils]
            [ubergraph.core :as uber]
            [clojure.test :refer [deftest are is]]
            [clojure.template :refer [do-template]]))


(deftest add-to-dom:tuple-spec
  (do-template [m]
      (let [{term :term spec :spec result :result} m]
        (is (= result (utils/env->map (sut/add-to-dom (uber/digraph) term spec)))))

      {:term     (ru/to-head-tail-list (r/->VarTerm "A") (r/->VarTerm "B"))
       :spec     (r/->TupleSpec [(r/->IntegerSpec) (r/->AtomSpec)])
       :result   {"[A, B]"    ["List(Any)" "Tuple(Integer, Atom)"]
                  "[B]"       ["List(Any)" "Tuple(Atom)"]
                  "A"         ["Integer" "Any"]
                  "B"         ["Any" "Atom"]
                  "[]"        ["Tuple()" "List(Any)"]
                  ["A" "[A, B]"]    :is-head
                  ["[B]" "[A, B]"]  :is-tail
                  ["B" "[B]"]       :is-head
                  ["[]" "[B]"]      :is-tail

                  }}

      {:term (r/->EmptyListTerm)
       :spec (r/->TupleSpec [])
       :result {"[]" ["Tuple()" "EmptyList"]}}

      ;;Does not throw an error at the moment, as we only collect
      {:term (r/->EmptyListTerm)
       :spec (r/->TupleSpec [(r/->IntegerSpec)])
       :result {"[]" ["Tuple(Integer)" "EmptyList"]}}
      ))

(deftest add-to-dom:compound-spec
  (do-template [m]
               (let [{term :term spec :spec result :result} m]
                 (is (= result (utils/env->map (sut/add-to-dom (uber/digraph) term spec)))))

               {:term (r/->CompoundTerm "foo" [(r/->VarTerm "A") (r/->VarTerm "B")])
                :spec (r/->CompoundSpec "foo" [(r/->IntegerSpec) (r/->AtomSpec)])
                :result {"Compound(foo(A, B))"   ["Compound(foo(Integer, Atom))" "Compound(foo(Any, Any))"]
                         "A"                     ["Integer" "Any"]
                         "B"                     ["Any" "Atom"]
                         ["A" "Compound(foo(A, B))"] :arg-at-pos
                         ["B" "Compound(foo(A, B))"] :arg-at-pos
                         }}
      ))

(deftest add-to-dom:list-spec
  (do-template [m]
      (let [{term :term spec :spec result :result} m]
        (is (= result (utils/env->map (sut/add-to-dom (uber/digraph) term spec)))))

      {:term     (ru/to-head-tail-list (r/->VarTerm "A") (r/->VarTerm "B"))
       :spec     (r/->ListSpec (r/->IntegerSpec))
       :result   {"[A, B]"    ["List(Any)" "List(Integer)"]
                  "[B]"       ["List(Any)" "List(Integer)"]
                  "A"         ["Integer" "Any"]
                  "B"         ["Integer" "Any"]
                  "[]"        ["List(Any)" "List(Integer)"]
                  ["A" "[A, B]"]    :is-head
                  ["[B]" "[A, B]"]  :is-tail
                  ["B" "[B]"]       :is-head
                  ["[]" "[B]"]      :is-tail

                  }}

      {:term (r/->EmptyListTerm)
       :spec (r/->TupleSpec [])
       :result {"[]" ["Tuple()" "EmptyList"]}}

      ;;Does not throw an error at the moment, as we only collect
      {:term (r/->EmptyListTerm)
       :spec (r/->TupleSpec [(r/->IntegerSpec)])
       :result {"[]" ["Tuple(Integer)" "EmptyList"]}}
      ))


(deftest add-to-dom:and
  (do-template [m]
               (let [{term :term spec :spec result :result} m]
                 (is (= result (utils/env->map (sut/add-to-dom (uber/digraph) term spec)))))

               {:term (r/->IntegerTerm 3)
                :spec (r/->AndSpec #{(r/->GroundSpec) (r/->NumberSpec)})
                :result {"3" ["Ground" "And(Ground, Number)" "Integer" "Number" "And(Number)"]}}

               ))

(deftest add-to-dom:or
  (do-template [m]
               (let [{term :term spec :spec result :result} m]
                 (is (= result (utils/env->map (sut/add-to-dom (uber/digraph) term spec)))))

               {:term (r/->IntegerTerm 3)
                :spec (r/->OneOfSpec #{(r/->GroundSpec)})
                :result {"3" ["OneOf(Ground)" "Integer"]}}

               ))
