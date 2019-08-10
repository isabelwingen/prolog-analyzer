(ns prolog-analyzer.analyzer.core-test
  (:require [prolog-analyzer.analyzer.core :as sut]
            [prolog-analyzer.parser :as parser]
            [prolog-analyzer.utils :as utils]
            [midje.sweet :refer :all]))


(def PATH "resources/test.tmp")

(def PREAMBLE ":- module(tmp,[]).\n:- use_module('../prolog/annotations',[spec_pre/2,spec_post/3,declare_spec/1,define_spec/2]).\n\n\n" )


(defn parse [path]
  (parser/process-prolog-file "swipl" "prolog/prolog_analyzer.pl" "swipl" path))

(defn parse-tmp [s]
  (spit PATH (str PREAMBLE s))
  (parser/process-prolog-file "swipl" "prolog/prolog_analyzer.pl" "swipl" PATH)
  )



(facts
 "Simple Example"
 (fact "First Example"
       (utils/env->map
        (first
         (sut/complete-analysis (parse-tmp
                                 (str
                                  "foo(X, Y) :- atom(X), bar(X, Y).\n"
                                  ":- spec_pre(bar/2, [int, atom]).\n"
                                  ":- spec_pre(bar/2, [atom, int]).\n"
                                  "bar(3, a).\n"
                                  "bar(a, 3).\n")))))
       => (contains {"X" "Atom"
                     "Y" "Integer"})))
(facts
 "Vars"
 (fact "int or var"
       (utils/env->map
        (first
         (sut/complete-analysis (parse-tmp
                                 (str
                                  ":- spec_pre(foo/1,[int]).\n"
                                  ":- spec_pre(foo/1,[var]).\n"
                                  "foo(E).")))))
       => (contains {"E" "OneOf(Integer, Var)"})
       )
 (fact "Three combinations"
       (utils/env->map
        (first
         (sut/complete-analysis (parse-tmp
                                 (str
                                  ":- spec_pre(foo/1,[var]).\n"
                                  "foo(E).")))))
       => (contains {"E" "Var"})
       (utils/env->map
        (first
         (sut/complete-analysis (parse-tmp
                                 (str
                                  ":- spec_pre(foo/1,[int]).\n"
                                  "foo(E).")))))
       => (contains {"E" "Integer"})

       (utils/env->map
        (first
         (sut/complete-analysis (parse-tmp
                                 (str
                                  ":- spec_pre(foo/1,[var]).\n"
                                  "foo(1).")))))
       => (contains {"1" "Integer"})
       )
 (fact "member"
       (utils/env->map
        (second
         (sut/complete-analysis (parse-tmp
                                 (str
                                  ":- spec_pre(mmember/2, [int, list(int)]).\n"
                                  ":- spec_pre(mmember/2, [var, list(int)]).\n"
                                  ":- spec_pre(mmember/2, [int, var]).\n"
                                  "mmember(H,[H|_]) :- !.\n"
                                  "mmember(E,[_|T]) :- mmember(E,T).\n")))))
       => (contains {"E" "OneOf(Integer, Var)"
                     "T" "OneOf(List(Integer), Var)"
                     "[E, T]" "OneOf(Tuple(Integer, List(Integer)), Tuple(Integer, Var), Tuple(Var, List(Integer)))"}))
 (fact "nothing"
       (utils/env->map
        (first
         (sut/complete-analysis
          (parse-tmp
           (str
            "foo(X).")))))
       => (contains {"X" "Any"})))


(facts
 "Check Built-Ins"
 (fact "Placeholder in sort"
       (utils/env->map (first (sut/complete-analysis (parse-tmp "foo(X) :- sort([1,2,a], X)."))))
       => (contains {"X" "List(OneOf(Integer, Atom))"}))
 (fact "Placeholder in member"
       (utils/env->map (first (sut/complete-analysis (parse-tmp "foo :- member(1,[a,b,c])."))))
       => (contains {"1" "ERROR: No valid intersection of Atom and Integer"})

       (utils/env->map (first (sut/complete-analysis (parse-tmp "foo(X) :- member(X,[p,b,2])."))))
       => (contains {"X" "OneOf(Integer, Atom)"})
       ))
