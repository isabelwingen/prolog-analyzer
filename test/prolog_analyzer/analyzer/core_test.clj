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
       (utils/env->map (nth (sut/complete-analysis (parse "resources/simple-example.pl")) 2))
       => (contains {"X" "Atom"
                     "Y" "Integer"})))


(facts
 "Check Built-Ins"
 (fact "Placeholder in sort"
       (utils/env->map (first (sut/complete-analysis (parse-tmp "foo(X) :- sort([1,2,a], X)."))))
       => (contains {"X" "List(OneOf(Integer, Atom))"})))
