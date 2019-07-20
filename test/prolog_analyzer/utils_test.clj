(ns prolog-analyzer.utils-test
  (:require [prolog-analyzer.utils :as sut]
            [prolog-analyzer.parser :as parser]
            [prolog-analyzer.records :as r]
            [ubergraph.core :as uber]
            [loom.graph]
            [ubergraph.protocols]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test :refer [deftest are is]]))



(def data (parser/process-prolog-file "swipl" "prolog/prolog_analyzer.pl" "swipl" "resources/spec-test.pl"))


(deftest get-specs-of-pred-test
  (is (= {:pre-specs [
                      [(r/->VarSpec) (r/->ListSpec (r/->IntegerSpec))]
                      [(r/->IntegerSpec) (r/->ListSpec (r/->IntegerSpec))]
                      ]
          :post-specs {[(r/->VarSpec)  (r/->ListSpec (r/->IntegerSpec))]
                       (r/->TupleSpec [(r/->IntegerSpec) (r/->ListSpec (r/->IntegerSpec))])}}
         (sut/get-specs-of-pred ["spec_test" "member_int" 2] data))))

(deftest get-pred-identities-test
  (is (= [["spec_test" "member_int" 2] ["spec_test" "foo" 3]] (sut/get-pred-identities data))))

(deftest get-clause-identities-test
  (is (=
       [[["spec_test" "member_int" 2] 0]
        [["spec_test" "member_int" 2] 1]
        [["spec_test" "foo" 3] 0]]
       (sut/get-clause-identities data))))

(deftest get-clause-identities-of-pred-test
  (is (= [0 1]
         (sut/get-clause-identities-of-pred ["spec_test" "member_int" 2] data)))
  (is (= [0]
         (sut/get-clause-identities-of-pred ["spec_test" "foo" 3] data))))

(deftest get-clauses-of-pred-test
  (is (= 2 (count (sut/get-clauses-of-pred ["spec_test" "member_int" 2] data))))
  (is (= 1 (count (sut/get-clauses-of-pred ["spec_test" "foo" 3] data)))))

(deftest get-terms-test
  (is (= [:a :b :c] (sut/get-terms (uber/digraph :a :b :c)))))

(deftest get-dom-of-term-test
  (is (= [{:spec :integer} {:spec :any}] (sut/get-dom-of-term (-> (uber/digraph) (uber/add-nodes-with-attrs [:a {:dom [{:spec :integer} {:spec :any}]}]))
                                                              :a (r/->AnySpec)))))


(deftest self-calling-test
  (is (false? (sut/self-calling? [["spec_test" "member_int" 2] 0] data)))
  (is (true? (sut/self-calling? [["spec_test" "member_int" 2] 1] data)))

  )
