(ns prolog-analyzer.utils-test
  (:require [prolog-analyzer.utils :as sut]
            [prolog-analyzer.parser :as parser]
            [ubergraph.core :as uber]
            [loom.graph]
            [ubergraph.protocols]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test :refer [deftest are is]]))



(def data (parser/process-prolog-file "resources/spec-test.pl"))

(deftest get-specs-of-pred-test
  (is (= {:pre-specs [[{:spec :integer} {:spec :list :type {:spec :integer}}]
                      [{:spec :var}, {:spec :list :type {:spec :integer}}]]
          :post-specs [[[{:spec :var} {:spec :list :type {:spec :integer}}]
                        [{:spec :integer}, {:spec :list :type {:spec :integer}}]]]
          :inv-specs [[{:spec :any} {:spec :ground}]]}
         (sut/get-specs-of-pred ["spec_test" "member_int" 2] data))))

(deftest get-pred-identities-test
  (is (= [["spec_test" "member_int" 2] ["spec_test" "foo" 3]] (sut/get-pred-identities data))))

(deftest get-clause-identities-test
  (is (=
       [["spec_test" "member_int" 2 0]
        ["spec_test" "member_int" 2 1]
        ["spec_test" "foo" 3 0]]
       (sut/get-clause-identities data))))

(deftest get-clause-identities-of-pred-test
  (is (= [["spec_test" "member_int" 2 0] ["spec_test" "member_int" 2 1]]
         (sut/get-clause-identities-of-pred ["spec_test" "member_int" 2] data)))
  (is (= [["spec_test" "foo" 3 0]]
         (sut/get-clause-identities-of-pred ["spec_test" "foo" 3] data))))

(deftest get-clauses-of-pred-test
  (is (= 2 (count (sut/get-clauses-of-pred ["spec_test" "member_int" 2] data))))
  (is (= 1 (count (sut/get-clauses-of-pred ["spec_test" "foo" 3] data)))))

(deftest empty-list?-test
  (is (true? (sut/empty-list? {:type :atomic :term "[]"})))
  (is (false? (sut/empty-list? {:type :atomic :term "."})))
  (is (false? (sut/empty-list? {:type :atom :term "[]"})))
  (is (false? (sut/empty-list? {:type :list :head {:spec :var :name "X"} :tail {:type :atomic :term "[]"}})))
  (is (false? (sut/empty-list? {:term "[|]" :type :atomic})))
  (is (false? (sut/empty-list? {:term "[]" :type :atom})))
  (is (false? (sut/empty-list? {:type :list :arglist []}))))

(deftest to-head-tail-list
  (are [x y] (= x (apply sut/to-head-tail-list y))
    {:type :atomic :term "[]"} []

    {:type :list :head {:type :integer :value 1} :tail {:type :atomic :term "[]"}} [{:type :integer :value 1}]

    {:type :list
     :head {:type :integer :value 1}
     :tail {:type :list :head {:type :integer :value 2} :tail {:type :atomic :term "[]"}}} [{:type :integer :value 1} {:type :integer :value 2}]
    ))

(deftest to-tuple-spec-test
  (are [x y] (= x (apply sut/to-tuple-spec y))
    {:spec :error :reason "Cannot build a tuple with zero arguments"} []
    {:spec :tuple :arglist [{:spec :integer}]} [{:spec :integer}]
    {:spec :tuple :arglist [{:spec :integer} {:spec :atom}]} [{:spec :integer} {:spec :atom}]))

(deftest to-or-spec-test
  (are [x y] (= x (apply sut/to-or-spec y))
    {:spec :error :reason "Cannot build empty one-of"} []
    {:spec :integer} [{:spec :integer}]
    {:spec :one-of :arglist [{:spec :integer} {:spec :atom}]} [{:spec :integer} {:spec :atom}]))


(deftest get-elements-of-list-test
  (is (= (list {:type :integer :value 1} {:type :integer :value 2} {:type :integer :value 3})
         (sut/get-elements-of-list {:type :list
                                    :head {:type :integer :value 1}
                                    :tail {:type :list
                                           :head {:type :integer :value 2}
                                           :tail {:type :list
                                                  :head {:type :integer :value 3}
                                                  :tail {:term "[]" :type :atomic}}}}))))

(deftest replace-specvar-name-with-value-test
  (are [original expected] (= expected (sut/replace-specvar-name-with-value original "Old" "New"))
    {:spec :specvar :name "Old"} {:spec :specvar :name "New"}
    {:spec :specvar :name "Unaffected"} {:spec :specvar :name "Unaffected"}
    {:spec :list :type {:spec :specvar :name "Old"}} {:spec :list :type {:spec :specvar :name "New"}}
    {:spec :one-of :arglist [{:spec :specvar :name "Old"} {:spec :specvar :name "Unaffected"} {:spec :integer}]} {:spec :one-of :arglist [{:spec :specvar :name "New"} {:spec :specvar :name "Unaffected"} {:spec :integer}]}
    {:spec :user-defined :name "foo" :arglist [{:spec :list :type {:spec :specvar :name "Old"}}]} {:spec :user-defined :name "foo" :arglist [{:spec :list :type {:spec :specvar :name "New"}}]}
    ))

(deftest replace-specvars-with-spec-test
  (are [original expected] (= expected (sut/replace-specvars-with-spec original "Old" {:spec :atom}))
    {:spec :specvar :name "Old"} {:spec :atom}
    {:spec :specvar :name "Unaffected"} {:spec :specvar :name "Unaffected"}
    {:spec :list :type {:spec :specvar :name "Old"}} {:spec :list :type {:spec :atom}}
    {:spec :one-of :arglist [{:spec :specvar :name "Old"} {:spec :specvar :name "Unaffected"} {:spec :integer}]} {:spec :one-of :arglist [{:spec :atom} {:spec :specvar :name "Unaffected"} {:spec :integer}]}
    {:spec :user-defined :name "foo" :arglist [{:spec :list :type {:spec :specvar :name "Old"}}]} {:spec :user-defined :name "foo" :arglist [{:spec :list :type {:spec :atom}}]}
    ))

(deftest find-specvars-test
  (are [spec expected] (= expected (sut/find-specvars spec))
    {:spec :specvar :name "X"} [{:spec :specvar :name "X"}]
    {:spec :one-of :arglist [{:spec :specvar :name "X"} {:spec :specvar :name "Y"} {:spec :specvar :name "Y"}]} [{:spec :specvar :name "X"} {:spec :specvar :name "Y"}]
    {:spec :user-defined :arglist [{:spec :specvar :name "X"} {:spec :specvar :name "Y"}]} [{:spec :specvar :name "X"} {:spec :specvar :name "Y"}]
    {:spec :one-of :arglist [{:spec :list :type {:spec :specvar :name "X"}} {:spec :specvar :name "Y"} {:spec :specvar :name "Y"}]} [{:spec :specvar :name "X"} {:spec :specvar :name "Y"}]
    {:spec :list :type {:spec :list :type {:spec :specvar :name "X"}}} [{:spec :specvar :name "X"}]
    ))

(deftest get-terms-test
  (is (= [:a :b :c] (sut/get-terms (uber/digraph :a :b :c :ENVIRONMENT)))))

(deftest get-all-specvars-in-doms-test
  (is (= [{:spec :specvar :name "X"} {:spec :specvar :name "Y"} {:spec :specvar :name "Z"} {:spec :specvar :name "A"}]
         (sut/get-all-specvars-in-doms (-> (uber/digraph) (uber/add-nodes-with-attrs [:a {:dom [{:spec :specvar :name "X"} {:spec :list :type {:spec :specvar :name "Y"}}]}]
                                                                                     [:b {:dom [{:spec :and :arglist [{:spec :specvar :name "Z"}]}]}]
                                                                                     [:d {:dom [{:spec :and :arglist [{:spec :integer}]}]}]
                                                                                     [:c {:dom [{:spec :tuple :arglist [{:spec :specvar :name "A"}]}]}]))))))

(deftest valid-env?-test
  (is (true? (sut/valid-env? (uber/digraph))))
  (is (false? (sut/valid-env? (-> (uber/digraph) (uber/add-nodes-with-attrs [:a {:dom [{:spec :error}]}])))))
  (is (false? (sut/valid-env? (-> (uber/digraph) (uber/add-nodes-with-attrs [:a {:dom [{:spec :error} {:spec :int}]}])))))
  (is (true? (sut/valid-env? (-> (uber/digraph) (uber/add-nodes-with-attrs [:a {:dom [{:spec :int} {:spec :atom}]}]))))))
