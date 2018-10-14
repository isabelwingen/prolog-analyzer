(ns prolog-analyzer.parser-test
  (:require [prolog-analyzer.parser :as sut]
            [clojure.test :as t]))


;; "foo(A,B) :- bar(B,A)."


(deftest parse-string-test
  (testing "Parsing of Prolog clauses"
    (are [x y] (= (sut/parse-string x) y)
      "foo(A,B)" [{:type :fact :value {:name "foo" :args [{:type :var :value "A"}, {:type :var :value "B"}]}}])))
