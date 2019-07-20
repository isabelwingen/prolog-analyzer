(ns prolog-analyzer.record-utils-test
  (:require [prolog-analyzer.record-utils :as sut]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.test-helper :refer [to-term to-spec]]
            [midje.sweet :refer :all]))


(facts
 "About simplify"
 (fact
  "OneOf"
  (sut/simplify (r/->OneOfSpec #{}) {}) => (just {:reason string?})
  (sut/simplify (r/->OneOfSpec #{(r/->IntegerSpec)}) {}) => (exactly (r/->IntegerSpec))
  (sut/simplify (r/->OneOfSpec #{(r/->OneOfSpec #{(r/->IntegerSpec) (r/->FloatSpec)}) (r/->AtomSpec)}) {})
  => (exactly   (r/->OneOfSpec #{(r/->IntegerSpec) (r/->FloatSpec) (r/->AtomSpec)}))
  (sut/simplify (r/->OneOfSpec #{(r/->TupleSpec [(r/->IntegerSpec)]) (r/->TupleSpec [(r/->FloatSpec)])}) {})
  =>   (exactly (r/->TupleSpec [(r/->OneOfSpec #{(r/->IntegerSpec) (r/->FloatSpec)})]))
  (sut/simplify (r/->OneOfSpec #{(r/->TupleSpec [(r/->IntegerSpec) (r/->FloatSpec)])
                                 (r/->TupleSpec [(r/->AtomSpec) (r/->FloatSpec)])}) {})
  =>   (exactly (r/->TupleSpec [(r/->OneOfSpec #{(r/->IntegerSpec) (r/->AtomSpec)}) (r/->FloatSpec)]))
  ))
