(ns prolog-analyzer.parser-test
  (:require [prolog-analyzer.parser :as sut]
            [clojure.test :refer :all]))


(def reference-map
  {"foo(a)" [:Fact [:Name "foo"] [:Args [:Atom "a"]]]
   "bar(b,X)" [:Fact [:Name "bar"] [:Args [:Atom "b"] [:Var "X"]]]})


(deftest parse-empty
  (testing "Parsing whitespace strings"
    (are [x] (= '() (sut/parse x))
      ""
      "\n"
      "\n\n"
      "%Normal Comment\n"
      "\n%Second Line Comment\n"
      "         "
      "\t\t\t")))

(deftest parse-with-comment
  (testing "Parsing text with comments"
    (are [x y] (= x (sut/parse y))
      '([:Fact [:Name "a"]], [:Fact [:Name "b"]])
      "a.\n%This is a comment\nb."
      '([:Fact [:Name "c"]], [:Fact [:Name "d"]], [:Fact [:Name "e"]])
      "c.%This is a comment at the end of line\n%Another comment\nd.%In-Line Comment\ne.\n\n"
      '([:Fact [:Name "f"]])
      "f.\n\n%End of File-Comment")))

(deftest parse-error
  (testing "Parsing things, that are wrong"
    (are [x] (contains? (sut/parse x) :reason)
      "a"
      "a.b."
      "a :- "
      "a :- ."
      "a(c,d"
      "a).")))

;; Test for parsing the component
(deftest parse-facts
  (testing "Simple Facts"
    (are [x y] (= x (sut/prolog-parser y :start :Fact))
      [:Fact [:Name "asdasdasd"]] "asdasdasd."
      [:Fact [:Name "foo"] [:Arglist [:Atom "a"]]] "foo(a)."
      [:Fact [:Name "bar"] [:Arglist [:Atom "b"] [:Var "X"]]] "bar(b,X).")))

(deftest parse-rules
  (testing "Simple Rules"
    (are [x y] (= x (sut/prolog-parser y :start :Rule))
      [:Rule [:Name "foo"] [:Arglist [:Atom "a"]]
       [:Goal [:Name "bar"] [:Arglist [:Atom "b"] [:Var "X"]]]]
      "foo(a) :- bar(b,X)."
      ;;
      [:Rule [:Name "foo"] [:Arglist [:Var "A"] [:Var "B"]]
       [:Goal [:Name "a"]]
       [:Semicolon]
       [:Goal [:Name "b"]]
       [:Komma]
       [:Goal [:Name "e"]]]
      "foo(A,B) :- a;b,e."
      )))

(deftest parse-directCall
  (testing "Parsing of direct calls"
    (are [x y] (= x (sut/prolog-parser y :start :DirectCall))
      [:DirectCall
       [:Goal [:Name "enable_all_specs"]]]
      ":- enable_all_specs."
      [:DirectCall
       [:Goal
        [:Name "assert"]
        [:Arglist
         [:Compound
          [:Functor "foo"] [:Arglist [:Atom "a"] [:Atom "b"]]]]]]
      ":- assert(foo(a,b))."
      )))

(deftest parse-if
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

 
 
