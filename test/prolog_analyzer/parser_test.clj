(ns prolog-analyzer.parser-test
  (:require [prolog-analyzer.parser :as sut]
            [prolog-analyzer.records :as r]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))

(def raw-data (sut/transform-to-edn "resources/tree-example.edn"))
raw-data
(def file "resources/tree-example.edn")

(sut/process-edn file)
