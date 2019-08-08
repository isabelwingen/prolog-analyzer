(ns prolog-analyzer.parser-test
  (:require [prolog-analyzer.parser :as sut]
            [prolog-analyzer.records :as r]
            [midje.sweet :refer :all]))

(defn f [path]
  (sut/process-prolog-file "swipl" "prolog/prolog_analyzer.pl" "swipl" path))

(facts
 (fact "About Post Specs"
       (:post-specs (f "resources/simple-example.pl"))
       =>
       (contains {["simple_example" "a" 1]    [{:guard [{:id 0 :type (r/->AnySpec)}]
                                                :conclusion [[{:id 0 :type (r/->AtomSpec)}]]}]
                  ["simple_example" "foo" 2]  [{:guard [{:id 0 :type (r/->AnySpec)} {:id 1 :type (r/->AnySpec)}]
                                                :conclusion [[{:id 0 :type (r/->IntegerSpec)} {:id 1 :type (r/->IntegerSpec)}] [{:id 0 :type (r/->AtomSpec)} {:id 1 :type (r/->AtomSpec)}]]}]}))
 (fact "About Inv Specs"
       (sut/process-prolog-file "swipl" "prolog/prolog_analyzer.pl" "swipl" "resources/simple-example.pl") => (contains {:inv-specs {}}))
 )
