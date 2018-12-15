(ns prolog-analyzer.domain-test
  (:require [prolog-analyzer.domain :as sut]
            [clojure.test :refer [deftest is are]]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            ))

(s/def :simple/arglist (s/and (s/coll-of ::simple-dom) #(< (count %) 4)))
(s/def ::simple-or-spec (s/keys :req-un [:or/spec :simple/arglist]))
(s/def ::simple-and-spec (s/keys :req-un [:and/spec :simple/arglist]))
(s/def ::simple-dom (s/or :normal ::sut/normal-spec
                          :or ::simple-or-spec
                          :and ::simple-and-spec))


(binding [s/*recursion-limit* 2] (gen/generate (s/gen ::simple-dom)))

(deftest to-knf-test
  (are [input expected] (= expected (sut/to-knf input))
    {:spec :a} {:spec :or :arglist [{:spec :a}]}
    {:spec :or :arglist [{:spec :a}]} {:spec :or :arglist [{:spec :or :arglist [{:spec :a}]}]}
    ))
