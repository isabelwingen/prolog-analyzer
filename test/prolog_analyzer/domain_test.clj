(ns prolog-analyzer.domain-test
  (:require [prolog-analyzer.domain :as sut]
            [clojure.test :refer [deftest is are]]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]

            [clojure.spec.test.alpha :as stest]
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
    {:spec :a} {:spec :a}
    {:spec :or :arglist [{:spec :a}]} {:spec :a}
    {:spec :and :arglist [{:spec :a}]} {:spec :a}
    {:spec :and :arglist [{:spec :a} {:spec :a}]} {:spec :a}
    {:spec :or :arglist [{:spec :a} {:spec :b} {:spec :a}]} {:spec :or :arglist [{:spec :a} {:spec :b}]}

    ;; (a or b) or (b or c) == a or b or c
    {:spec :or :arglist [{:spec :or :arglist [{:spec :a} {:spec :b}]}
                         {:spec :or :arglist [{:spec :b} {:spec :c}]}]}
    {:spec :or :arglist [{:spec :a} {:spec :b} {:spec :c}]}

    ;; (a or b) and (b or c)
    {:spec :and :arglist [{:spec :or :arglist [{:spec :a} {:spec :b}]}
                         {:spec :or :arglist [{:spec :b} {:spec :c}]}]}
    {:spec :or :arglist [{:spec :and :arglist [{:spec :a} {:spec :b}]}
                         {:spec :and :arglist [{:spec :a} {:spec :c}]}
                         {:spec :b}
                         {:spec :and :arglist [{:spec :b} {:spec :c}]}
                         ]}

    {:spec :and :arglist [{:spec :a} {:spec :b} {:spec :a}]} {:spec :or :arglist [{:spec :and :arglist [{:spec :a} {:spec :b}]}]}
    ))

(deftest knf-test
  (are [x] (sut/knf? (sut/to-knf x))
    {:spec :a}
    {:spec :or :arglist [{:spec :a} {:spec :b}]}
    {:spec :or :arglist [{:spec :and :arglist [{:spec :a} {:spec :b}]} {:spec :c}]}
    ))


(s/fdef to-knf-wrapper
        :args (s/cat :dom ::simple-dom)
        :ret ::sut/dom
        :fn (s/and #(sut/knf? (:ret %))))

(defn to-knf-wrapper [dom]
  (sut/to-knf dom))

;(binding [s/*recursion-limit* 1] (stest/abbrev-result (first (stest/check `to-knf-wrapper))))
