(ns prolog-analyzer.analyzer.global-analysis-test
  (:require [prolog-analyzer.analyzer.global-analysis :as sut]
            [clojure.test :refer [deftest is are]]
            [prolog-analyzer.parser :as parser]))


(defn get-data [] (parser/process-prolog-file "swipl" "prolog/prolog_analyzer.pl" "swipl" "resources/global2.pl"))

(defn execute-step [data]
  (let [envs (sut/step data)
        new-data (sut/add-new-knowledge data envs)]
    new-data))
