(ns prolog-analyzer.utils-test
  (:require [prolog-analyzer.utils :as sut]
            [prolog-analyzer.parser :as parser]
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

(deftest get-impls-of-pred-test
  (is (= 2 (count (sut/get-impls-of-pred ["spec_test" "member_int" 2] data))))
  (is (= 1 (count (sut/get-impls-of-pred ["spec_test" "foo" 3] data)))))

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
