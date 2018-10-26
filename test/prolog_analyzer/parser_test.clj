(ns prolog-analyzer.parser-test
  (:require [prolog-analyzer.parser :as sut]
            [clojure.pprint :refer [pprint]]
            [instaparse.core :as insta]
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
(deftest parse-wrong
  (testing "result parsing wrong things"
    (are [x] (contains? (sut/parse x) :reason)
      "a :-"
      "foo(a,b)"
      "a.b"
      "a :- b"
      "a.b.c"
      "foo(a,)."
      "foo(a,b"
      "foo a b.")))

(deftest parse-with-comment
  (testing "Parsing text with comments"
    (are [x y] (= y (sut/parse x))
      "a.\n%This is a comment\nb."
      '([:Fact [:Name "a"]], [:Fact [:Name "b"]])
      "c.%This is a comment at the end of line\n%Another comment\nd.%In-Line Comment\ne.\n\n"
      '([:Fact [:Name "c"]], [:Fact [:Name "d"]], [:Fact [:Name "e"]])
      "f.\n\n%End of File-Comment"
      '([:Fact [:Name "f"]])
      ":- module(a, [arg1,
                     % comment
                     arg2])."
      '([:DirectCall
         [:Goal
          [:Name "module"]
          [:Arglist [:Atom "a"] [:List [:ExplicitList [:Atom "arg1"] [:Atom "arg2"]]]]]])
      "foo(a,
          % comment
          b)."
      '([:Fact [:Name "foo"] [:Arglist [:Atom "a"] [:Atom "b"]]])
      "/* the c interface */"
      '()
      )))

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
  (testing "Parsing of If"
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

(deftest parse-special-direct-call
  (are [y x] (= x (sut/prolog-parser y :start :DirectCall))
    ":- dynamic foo/2, bar/3."
    [:DirectCall
     [:Dynamic
      [:Declaration [:Name "foo"] [:Arity "2"]]
      [:Declaration [:Name "bar"] [:Arity "3"]]]]
    ":- meta_predicate cltl2ba(+,+), bar."
    [:DirectCall
     [:Metapredicate
      [:Metagoal [:Name "cltl2ba"] [:Arglist [:Atom "+"] [:Atom "+"]]]
      [:Metagoal [:Name "bar"]]
      ]]))

(deftest parse-abnormalities
  (are [y x] (= x (sut/parse y))
    "foo(+term)."
    '([:Fact [:Name "foo"] [:Arglist [:Compound [:SpecialChar_Compound "+"] [:Atom "term"]]]])
    "cltl2ba(+,+)."
    '([:Fact [:Name "cltl2ba"] [:Arglist [:Atom "+"] [:Atom "+"]]])
    "foo(a,(b,c))."
    '([:Fact [:Name "foo"] [:Arglist [:Atom "a"] [:Inner [:Atom "b"] [:Atom "c"]]]])
    "foo :- a -> b; c."
    '([:Rule [:Name "foo"]
       [:Goal [:Name "a"]]
       [:IfArrow]
       [:Goal [:Name "b"]]
       [:Semicolon]
       [:Goal [:Name "c"]]])
    "foo :- (2 < 3 -> a;b)."
    '([:Rule
       [:Name "foo"]
       [:Goal
        [:If 
         [:Goal [:BoolExpr [:Number "2"] [:Number "3"]]]
         [:Then]
         [:Goal [:Name "a"]]
         [:Else]
         [:Goal [:Name "b"]]]]])))

(sut/prolog-parser "(2<3 -> b;c)" :start :If)

(defn- test-source [source]
  (let [cat (slurp source)
        result (sut/parse cat)
        lines (clojure.string/split-lines cat)]
    (if (insta/failure? result)
      (pr-str (get lines (dec (:line result))))
      true)))


;; Todo: Improve handling
;; Provide prolog-source-files.txt which contains URLs/Paths of Prolog source files
(defn- parse-real-code []
  (let [l (clojure.string/split-lines (try
                                        (slurp "resources/prolog-source-files.txt")
                                        (catch Exception e "")))]
    (doseq [line (take 5 l)]
      (let [result (test-source line)]
        (is (true? result) (str "error in file " line " in line " result))))))

(parse-real-code)

(sut/parse "foo :- goal(a,b).")
(def x
  "timer_det_call(PP,Call) :- %print(call(PP,Call)),nl,
                              start_timer(PP,entry),
                              if(call(Call),stop_timer(PP,success),
                                            (print(det_call_failed(PP,Call)),nl,
                                            stop_timer(PP,fail))).")

(sut/parse x)
