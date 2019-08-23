(ns prolog-analyzer.analyzer.global-analysis-test
  (:require [prolog-analyzer.analyzer.global-analysis :as sut]
            [prolog-analyzer.parser :as parser]
            [prolog-analyzer.state :as state]
            [prolog-analyzer.records :as r]
            [midje.sweet :refer :all]))

(def d (parser/process-prolog-file "swipl" "prolog/prolog_analyzer.pl" "swipl" "resources/abs_int.pl"))

(def p (sut/global-analysis (fn [x] nil) d))

#_(doseq [[x y] (@state/user-typedefs)]
  (println (r/to-string x) (r/to-string y)))
