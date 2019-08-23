(ns prolog-analyzer.record-utils-test
  (:require [prolog-analyzer.record-utils :as sut]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.test-helper :refer [to-term to-spec]]
            [prolog-analyzer.state :as state]
            [midje.sweet :refer :all]))


(facts
 "About simplify"
 (fact
  "OneOf"
  (sut/simplify (r/->OneOfSpec #{}) true) => (just {:reason string?})

  (sut/simplify (r/->OneOfSpec #{(r/->IntegerSpec)}) true) => (exactly (r/->IntegerSpec))

  (sut/simplify (r/->OneOfSpec #{(r/->OneOfSpec #{(r/->IntegerSpec) (r/->FloatSpec)}) (r/->AtomSpec)}) true)
  => (exactly   (r/->OneOfSpec #{(r/->IntegerSpec) (r/->FloatSpec) (r/->AtomSpec)}))

  (sut/simplify (r/->OneOfSpec #{(r/->TupleSpec [(r/->IntegerSpec)])
                                   (r/->TupleSpec [(r/->FloatSpec)])}) true)
  =>   (exactly (r/->TupleSpec [(r/->OneOfSpec #{(r/->IntegerSpec) (r/->FloatSpec)})]))

  (sut/simplify (r/->OneOfSpec #{(r/->TupleSpec [(r/->IntegerSpec) (r/->FloatSpec)])
                                 (r/->TupleSpec [(r/->AtomSpec) (r/->FloatSpec)])}) true)
  =>   (exactly (r/->TupleSpec [(r/->OneOfSpec #{(r/->IntegerSpec) (r/->AtomSpec)}) (r/->FloatSpec)]))
  ))


(facts
 "About intersect"
 (fact
  "Placeholder"
  (sut/intersect (r/->ListSpec (r/->PlaceholderSpec "a")) (r/->TupleSpec [(r/->IntegerSpec) (r/->AtomSpec)]) false) => (r/->TupleSpec [(assoc (r/->PlaceholderSpec "a") :alias (r/->IntegerSpec)) (assoc (r/->PlaceholderSpec "a") :alias (r/->AtomSpec))])))


(facts
 "About non-empty intersect"
 (fact
  (sut/non-empty-intersection (r/->VarSpec) (r/->OneOfSpec #{(r/->IntegerSpec) (r/->VarSpec)}) false)
  => true))

;:- define_spec(expr, one_of([atom, compound(expr(atom,expr,expr))])).

(defn f []
  (reset! state/grounded {})
  (reset! state/user-typedefs {(r/->UserDefinedSpec "expr")
                               (r/->OneOfSpec #{(r/->AtomSpec)
                                                (r/->CompoundSpec "expr" [(r/->AtomSpec) (r/->UserDefinedSpec "expr") (r/->UserDefinedSpec "expr")])
                                                (r/->CompoundSpec "neg" [(r/->UserDefinedSpec "expr")])})

                               (r/->UserDefinedSpec "bla")
                               (r/->OneOfSpec #{(r/->AtomSpec) (r/->VarSpec) (r/->CompoundSpec "bla" [(r/->UserDefinedSpec "bla")])})
                               }))
