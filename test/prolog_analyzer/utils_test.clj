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



(def data (parser/process-prolog-file "resources/spec-test.pl"))

(deftest get-specs-of-pred-test
  (is (= {:pre-specs [[(r/make-spec:integer) (r/make-spec:list (r/make-spec:integer))]
                      [(r/make-spec:var) (r/make-spec:list (r/make-spec:integer))]]
          :post-specs [[[(r/make-spec:var)  (r/make-spec:list (r/make-spec:integer))]
                        [(r/make-spec:integer) (r/make-spec:list (r/make-spec:integer))]]]
          :inv-specs [[(r/make-spec:any) (r/make-spec:ground)]]}
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

(deftest get-dom-of-term-test
  (is (= [{:spec :integer} {:spec :any}] (sut/get-dom-of-term (-> (uber/digraph) (uber/add-nodes-with-attrs [:a {:dom [{:spec :integer} {:spec :any}]}]))
                                                              :a))))
