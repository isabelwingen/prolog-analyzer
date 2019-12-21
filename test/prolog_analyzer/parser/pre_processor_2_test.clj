(ns prolog-analyzer.parser.pre-processor-2-test
  (:require [prolog-analyzer.parser.pre-processor-2 :as sut]
            [prolog-analyzer.core :refer [run]]
            [midje.sweet :refer :all]))

(def data (run "resources/modules"))

(facts "Module Resolution"
 (fact "foo in b"
       (get-in data [:preds ["b" "foo" 2] 0 :body])
       => (just (contains {:module "b"}) (contains {:module "e"})))
 (fact "yoyo in b"
       (get-in data [:preds ["b" "yoyo" 1] 0 :body])
       => (just (contains {:module "d"})))
 )

(facts "Overview"
       (fact :pre-specs
             (-> data
                 :pre-specs
                 count)
             => 138)
       (fact :post-specs
             (-> data
                 :post-specs
                 count)
             => 89)
       (fact :preds
             (-> data
                 :preds
                 count)
             => 7))
