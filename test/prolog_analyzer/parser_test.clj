(ns prolog-analyzer.parser-test
  (:require [prolog-analyzer.parser :as sut]
            [clojure.test :as t]))


(def reference-map
  {"foo(a)" [:Fact [:Name "foo"] [:Args [:Atom "a"]]]
   "bar(b,X)" [:Fact [:Name "bar"] [:Args [:Atom "b"] [:Komma] [:Var "X"]]]})



(deftest parse-prolog1
  (testing "Simple Facts"
    (are [x y] (= x (sut/prolog-parser y :start :Fact))
      [:Fact [:Name "asdasdasd"]] "asdasdasd."
      [:Fact [:Name "foo"] [:Args [:Atom "a"]]] "foo(a)."
      [:Fact [:Name "bar"] [:Args [:Atom "b"] [:Komma] [:Var "X"]]] "bar(b,X).")))

(deftest parse-prolog2
  (testing "Simple Rules"
    (are [x y] (= x (sut/prolog-parser y :start :Rule))
      [:Rule
       [:Goal [:Name "foo"] [:Args [:Atom "a"]]]
       [:StartOfBody]
       [:Goal [:Name "bar"] [:Args [:Atom "b"] [:Komma] [:Var "X"]]]]
      "foo(a) :- bar(b,X)."
      ;;
      [:Rule
       [:Goal [:Name "foo"] [:Args [:Var "A"] [:Komma] [:Var "B"]]]
       [:StartOfBody]
       [:Goal [:Name "a"]]
       [:Semicolon]
       [:Goal [:Name "b"]]
       [:Komma]
       [:Goal [:Name "e"]]]
      "foo(A,B) :- a;b,e."
      )))

(deftest parse-prolog3
  (testing "If"
    (are [y x] (= x (sut/prolog-parser y :start :If))
      "(a -> c ; d)"
      [:If
       [:Goal [:Name "a"]]
       [:Then]
       [:Goal [:Name "c"]]
       [:Else]
       [:Goal [:Name "d"]]]
      "(a -> (f;e); x)"
      [:If
       [:Goal [:Name "a"]]
       [:Then]
       [:InBrackets
        [:Goal [:Name "f"]]
        [:Semicolon]
        [:Goal [:Name "e"]]]
       [:Else]
       [:Goal [:Name "x"]]]
      "(a -> (b -> c;d); e)"
      [:If
       [:Goal [:Name "a"]]
       [:Then]
       [:Goal
        [:If
         [:Goal [:Name "b"]]
         [:Then]
         [:Goal [:Name "c"]]
         [:Else]
         [:Goal [:Name "d"]]]]
       [:Else]
       [:Goal [:Name "e"]]]
      )))

