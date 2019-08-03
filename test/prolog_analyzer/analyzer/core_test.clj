(ns prolog-analyzer.analyzer.core-test
  (:require [prolog-analyzer.analyzer.core :as sut]
            [prolog-analyzer.parser :as parser]
            [prolog-analyzer.utils :as utils]
            [midje.sweet :refer :all]))



(defn parse [path]
  (parser/process-prolog-file "swipl" "prolog/prolog_analyzer.pl" "swipl" path))

(facts
 "Simple Example"
 (fact "First Example"
       (utils/env->map (nth (sut/complete-analysis (parse "resources/simple-example.pl")) 2))
       => (contains {"X" "Atom"
                     "Y" "Integer"})))
