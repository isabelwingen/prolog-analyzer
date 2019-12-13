(ns prolog-analyzer.parser.pre-processor-test
  (:require [prolog-analyzer.parser.pre-processor :as sut]
            [prolog-analyzer.core :refer [run]]
            [midje.sweet :refer :all]))

(def data (run "resources/modules"))

(-> data
    :preds
    (get ["b" "foo" 2])
    (get 0)
    (get :body))

(facts "Module Resolution"
 (fact "foo in b"
       (get-in data [:preds ["b" "foo" 2] 0 :body])
       => (just (contains {:module "b"}) (contains {:module "e"})))
 (fact "yoyo in b"
       (get-in data [:preds ["b" "yoyo" 1] 0 :body])
       => (just (contains {:module "d"})))
 )
