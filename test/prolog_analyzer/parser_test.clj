(ns prolog-analyzer.parser-test
  (:require [prolog-analyzer.parser :as sut]
            [clojure.test :as t]))






(deftest parse-string-test
  (testing "Parsing of Prolog clauses"
    (are [x y] (= x (sut/prolog-parser y))
      '({:tag :Fact
         :content ({:tag :Name :content ("foo")}
                   {:tag :Args :content ({:tag :Var :content ("A")} {:tag :Var :content ("B")})})})
      ;({:type :Fact :content ({:tag :Name :content ("foo")}{:tag :Args :content ({:tag :Var :content ("A")} {:tag :Var :content ("B")})})})
      "foo(A,B)."
      '({:tag :Fact :content ({:tag :Name :content ("a")})})
      "a."
      )))
