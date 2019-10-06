(ns prolog-analyzer.analyzer.core-test
  (:require [prolog-analyzer.analyzer.core :as sut]
            [prolog-analyzer.parser.parser :as parser]
            [prolog-analyzer.utils :as utils]
            [clojure.tools.namespace.reload :as p]
            [clojure.java.io :as io]
            [midje.sweet :refer :all]))

(def executor (atom {}))

(def BUILT-IN "edns/builtins.edn")

(def PREAMBLE ":- module(tmp,[]).\n:- use_module('../prolog/annotations',[spec_pre/2,spec_post/3,declare_spec/1,define_spec/2]).\n\n\n" )

(defn parse-tmp [h s]
  (Thread/sleep 500)
  (let [path "resources/test/core_tmp.pl"
        res (do (spit path (str PREAMBLE s))
                (parser/process-prolog-file "swipl" "prolog/prolog_analyzer.pl" "swipl" path))]
    (when (.exists (io/file path))
      (io/delete-file (io/file path)))
    (when (.exists (io/file "edns/core_tmp.edn"))
      (io/delete-file (io/file "edns/core_tmp.edn")))
    res))

(when (not (.exists (io/file BUILT-IN)))
  (println "Create edn")
  (parse-tmp "1" "foo.")
  (Thread/sleep 5000))

(defn analyze [s & strs]
  (let [string (apply str s strs)
        h (hash string)]
    (->> string
         (parse-tmp h)
         sut/complete-analysis
         first
         utils/env->map)))

(io/make-parents "resources/test/.ignore")

(facts
 "Build-ins"
 (fact "atom"
       (analyze "foo(X) :- atom(X).")
       => (contains {"X" "Atom"})))

(facts
 "Simple Example"
 (fact "First Example"
       (analyze
          "foo(X, Y) :- atom(X), bar(X, Y).\n"
          ":- spec_pre(bar/2, [int, atom]).\n"
          ":- spec_pre(bar/2, [atom, int]).\n"
          "bar(3, a).\n"
          "bar(a, 3).\n")
       => (contains {"X" "Atom"
                     "Y" "Integer"})))

(facts
 "Vars"
 (fact "int or var"
       (analyze
        ":- spec_pre(foo/1,[int]).\n"
        ":- spec_pre(foo/1,[var]).\n"
        "foo(E).")
       => (contains {"E" "OneOf(Integer, Var)"})
       )
 (fact "Three combinations"
       (analyze
        ":- spec_pre(foo/1,[var]).\n"
        "foo(E).")
       => (contains {"E" "Var"})
       (analyze
        ":- spec_pre(foo/1,[int]).\n"
        "foo(E).")
       => (contains {"E" "Integer"})

       (analyze
        ":- spec_pre(foo/1,[var]).\n"
        "foo(1).")
       => (contains {"1" "Integer"})
       )
 (fact "member"
       (analyze
        ":- spec_pre(mmember/2, [int, list(int)]).\n"
        ":- spec_pre(mmember/2, [var, list(int)]).\n"
        ":- spec_pre(mmember/2, [int, var]).\n"
        ;"mmember(H,[H|_]) :- !.\n"
        "mmember(E,[_|T]) :- mmember(E,T).\n")
       => (contains {"E" "OneOf(Integer, Var)"
                     "T" "List(Integer)"
                     "[E, T]" "Tuple(OneOf(Integer, Var), List(Integer))"}))
 (fact "nothing"
       (analyze "foo(X).")
       => (contains {"X" "Any"})))


(facts
 "Check Built-Ins"
 (fact "Placeholder in sort"
       (analyze "foo(X) :- sort([1,2,a], X).")
       => (contains {"X" "List(OneOf(Atom, Integer))"}))
 (fact "Placeholder in member"
       (analyze "foo :- member(1,[a,b,c]).")
       => (contains {"1" "ERROR: No valid intersection of Atom and Integer"}))
 (fact "Placeholder in member 2"
       (analyze "foo(X) :- member(X,[p,b,2]).")
       => (contains {"X" "OneOf(Atom, Integer)"})))


(facts
 "Check Postspecs"
 (fact "Simple - Valid"
       (analyze
        ":- spec_post(foo/2, [0:int], [[1:atom]]).\n"
        "goo(X) :- foo(1,X).")
       => (contains {"X" "Atom"})
       (analyze
        ":- spec_post(foo/2, [0:int], [[1:atom], [1:float]]).\n"
        "goo(X) :- foo(1,X).")
       => (contains {"X" "OneOf(Atom, Float)"})
       (analyze
        ":- spec_post(foo/2, [0:list(int)], [[1:list(atom)]]).\n"
        "goo(X) :- foo([1,2,3],X).")
       => (contains {"X" "List(Atom)"}))
 (fact "Simple - Not valid"
       (analyze
        ":- spec_post(foo/2, [0:int], [[1:atom]]).\n"
        "goo(X) :- foo(a,X).")
       => (contains {"X" "Any"})
       (analyze
        ":- spec_post(foo/2, [0:list(atom)], [[1:list(atom)]]).\n"
        "goo(X) :- foo([1,2,3],X).")
       => (contains {"X" "Any"}))
 (fact "Placeholder - Valid"
       (analyze
        ":- spec_post(foo/2, [0:placeholder(a)], [[1:placeholder(a)]]).\n"
        "goo(X) :- foo(1, X).")
       => (contains {"X" "Integer"})
       (analyze
        ":- spec_post(foo/3, [0:placeholder(a),1:placeholder(b)], [[2:one_of([placeholder(a), placeholder(b)])]]).\n"
        "goo(A) :- foo(1,a,A).")
       => (contains {"A" "OneOf(Atom, Integer)"})
       (analyze
        ":- spec_post(foo/3, [0:placeholder(a),1:placeholder(b)], [[2:placeholder(a)], [2:placeholder(b)]]).\n"
        "goo(A) :- foo(1,a,A).")
       => (contains {"A" "OneOf(Atom, Integer)"})
       (analyze
        ":- spec_post(foo/3, [0:placeholder(a)], [[1:placeholder(a)]]).\n"
        ":- spec_post(foo/3, [0:int], [[2:float]]).\n"
        "goo(A) :- foo(1,A,B).")
       => (contains {"A" "Integer"
                     "B" "Float"})
       (analyze
        ":- spec_post(foo/2, [0:list(placeholder(a))], [[1:placeholder(a)]]).\n"
        "goo(X) :- foo([1,2,a], X).")
       => (contains {"X" "OneOf(Atom, Integer)"}))
 (fact "Placeholder - Invalid"
       (analyze
        ":- spec_post(foo/2, [0:list(placeholder(a))], [[1:placeholder(a)]]).\n"
        "goo(X) :- foo(1, X).")
       => (contains {"X" "Any"})
       (analyze
        ":- spec_post(foo/2, [0:list(placeholder(a))], [[1:placeholder(a)]]).\n"
        "goo(X) :- foo(bla(1,2), X).")
       => (contains {"X" "Any"})
       )
 )
