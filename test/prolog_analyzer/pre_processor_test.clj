(ns prolog-analyzer.pre-processor-test
  (:require [prolog-analyzer.pre-processor :as sut]
            [prolog-analyzer.parser :as parser]
            [clojure.test :as t]))


(def data (-> "resources/module1.edn"
              parser/transform-to-edn))
data