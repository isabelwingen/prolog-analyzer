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
       (contains {["simple_example" "foo" 2]  [{:guard []
                                                :conclusion [[{:id 0 :type (r/->IntegerSpec)} {:id 1 :type (r/->IntegerSpec)}] [{:id 0 :type (r/->AtomSpec)} {:id 1 :type (r/->AtomSpec)}]]}]}))
 )

(def x (f "resources/simple-example.pl"))

(-> (f "resources/simple-example.pl")
    :post-specs
    (get ["user" "member" 2])
    first)
