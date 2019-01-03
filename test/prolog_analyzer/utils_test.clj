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

(deftest get-pred-identities-test
  (is (= [["spec_test" "member_int" 2] ["spec_test" "foo" 3]] (sut/get-pred-identities data))))

(deftest get-clause-identities-test
  (is (=
       [["spec_test" "member_int" 2 0]
        ["spec_test" "member_int" 2 1]
        ["spec_test" "foo" 3 0]]
       (sut/get-clause-identities data))))

(deftest get-clauses-of-pred-test
  (is (= 2 (count (sut/get-clauses-of-pred ["spec_test" "member_int" 2] data))))
  (is (= 1 (count (sut/get-clauses-of-pred ["spec_test" "foo" 3] data)))))

(deftest get-specs-of-pred-test
  (let [{pre-specs :pre-specs post-specs :post-specs} (sut/get-specs-of-pred ["spec_test" "member_int" 2] data)]
    (is (= 2 (count pre-specs)))
    (is (= (list {:spec :integer} {:spec :list :type {:spec :integer}})
           (first pre-specs)))
    (is (= (list {:spec :var} {:spec :list :type {:spec :integer}})
           (second pre-specs)))
    (is (= 1 (count post-specs)))
    (is (= (list (list {:spec :var} {:spec :list :type {:spec :integer}})
                 (list {:spec :integer} {:spec :list :type {:spec :integer}}))
           (first post-specs)))
    ))

(deftest empty-list-test
  (is (true? (sut/empty-list? {:term "[]" :type :atomic})))
  (is (false? (sut/empty-list? {:term "[|]" :type :atomic})))
  (is (false? (sut/empty-list? {:term "[]" :type :atom})))
  (is (false? (sut/empty-list? {:type :list :arglist []}))))

(deftest head-tail-list-to-list-test
  (is (= {:type :list :elements [{:type :integer :value 1} {:type :integer :value 2}]}
         (sut/head-tail-list-to-list {:type :list
                                      :head {:type :integer :value 1}
                                      :tail {:type :list
                                             :head {:type :integer :value 2}
                                             :tail {:term "[]" :type :atomic}}}))))

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


(def transitiv-closure-prop
    (prop/for-all [g graph-gen]
                  (= (transitive-closure-ref-impl g) (sut/transitive-closure g))))

(tc/quick-check 500 transitiv-closure-prop)
