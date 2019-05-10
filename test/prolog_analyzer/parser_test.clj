(ns prolog-analyzer.parser-test
  (:require [prolog-analyzer.parser :as sut]
            [prolog-analyzer.records :as r]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))

(defn get-data []
  (sut/process-prolog-file "swipl" "prolog/prolog_analyzer.pl" "swipl" "resources/spec-test.pl"))

(deftest spec-test
  (is (= 18 (count (keys (:specs (get-data)))))))
