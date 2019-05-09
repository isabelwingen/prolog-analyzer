(ns prolog-analyzer.core-test
  (:require [clojure.test :refer [deftest is are testing]]
            [prolog-analyzer.core :as sut]))


(deftest test-whole-program
  (sut/run "swipl" "prolog/prolog_analyzer.pl" "resources" "swipl"))
