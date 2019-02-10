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
(defn transitive-closure-ref-impl [g & attrs]
  (let [tmp-graph (atom g)]
    (doseq [k (uber/nodes @tmp-graph)]
      (doseq [i (uber/nodes @tmp-graph)]
        (if (uber/find-edge @tmp-graph i k)
          (doseq [j (uber/nodes @tmp-graph)]
            (if (uber/find-edge @tmp-graph k j)
              (swap! tmp-graph uber/add-edges [i,j]))))))
    @tmp-graph))

(def edge-gen
  (gen/not-empty (gen/vector (gen/tuple gen/int gen/int))))

(def graph-gen
  (gen/fmap #(apply uber/digraph %) edge-gen))

