(ns prolog-analyzer.analyzer.post-specs-test
  (:require [prolog-analyzer.analyzer.post-specs :as sut]
            [ubergraph.core :as uber]
            [prolog-analyzer.records :as r]
            [midje.sweet :refer :all]))




(facts
 "About regist-post-specs"
 (fact
  (uber/attr
   (sut/register-post-specs (uber/digraph) [:a :b :c] [{:guard [{:id 0 :type :x} {:id 1 :type :y}] :conclusion [{:id 2 :type :z}]}
                                                       {:guard [{:id 2 :type :p}] :conclusion [{:id 0 :type :q} {:id 1 :type :q}]}])
   :environment
   :post-specs)
  => (contains [{:conclusion [{:arg :c :type :z}] :guard [{:arg :a :type :x} {:arg :b :type :y}]}
                {:conclusion [{:arg :a :type :q} {:arg :b :type :q}] :guard [{:arg :c :type :p}]}])))

(def p (r/->NumberSpec))

(facts
 "apply-post-specs"
 (fact
  (sut/apply-post-specs (sut/register-post-specs (-> (uber/digraph) (uber/add-nodes-with-attrs [:X {:dom (r/->AtomSpec)}] [:Y {:dom (r/->IntegerSpec)}]))
                                                 [:X :Y] [{:guard [{:id 0 :type (r/->AtomSpec)}] :conclusion [{:id 1 :type (r/->NumberSpec)}]}]))
  => (contains [:Y p])))
