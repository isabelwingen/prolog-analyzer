(ns prolog-analyzer.analyzer.global-analysis-test
  (:require [prolog-analyzer.analyzer.global-analysis :as sut]
            [clojure.test :refer [deftest is are]]
            [prolog-analyzer.parser :as parser]))


(defn get-data [] (parser/process-prolog-file "swipl" "prolog/prolog_analyzer.pl" "swipl" "resources/global2.pl"))

(defn execute-step [data]
  (let [envs (sut/step data)
        new-data (sut/add-new-knowledge data envs)]
    new-data))


(map sut/pretty-str-postspec (-> (get-data)
                                 execute-step
                                 execute-step
                                 :post-specs
                                 (get ["global2" "foo" 3])))
