(ns prolog-analyzer.analyzer.calculate-env-test
  (:require [prolog-analyzer.analyzer.calculate-env :as sut]
            [prolog-analyzer.test-helper :refer [to-term to-spec] :as th]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [prolog-analyzer.utils :as utils]
            [midje.sweet :refer :all]))

(defmacro test-wrapper [header-vars spec]
  `(utils/env->map (sut/get-env-for-head "test" (apply vector (map to-term ~header-vars)) (to-spec ~spec))))

(facts
 "About Lists"
 (fact
  "Pass domain of head upwards"
  (test-wrapper
     ["H" "[H|T]"]
     ("Tuple" ["Integer" ("OneOf" [("List" ("OneOf" ["Integer" "Atom"]))
                                   ("List" "Float")])]))
  =>
  (contains {"H" "Integer"
             "T" "List(OneOf(Integer, Atom))"
             "[H|T]" "List(OneOf(Integer, Atom))"}))
 (fact
  "Pass domain of head upwards and pass it down to tail"
  (test-wrapper
   ["A" "[A, B]"]
   ("Tuple" ["Integer" ("OneOf" [("Tuple" ["Integer" "Atom"])
                                 ("Tuple" ["Float" "Float"])])]))
  =>
  (contains {"A" "Integer"
             "B" "Atom"}))
 (fact
  "Pass domain of tail upwards"
  (test-wrapper
   ["A" "[B]" "[A, B]"]
   ("Tuple" ["Float" ("Tuple" ["Integer"]) "Any"]))
  =>
  (contains {"A" "Float"
             "B" "Integer"
             "[B]" "Tuple(Integer)"
             "[A, B]" "Tuple(Float, Integer)"})
  (test-wrapper
     ["[B]" "[A, B]"]
     ("Tuple"
      [("Tuple" ["Integer"])
       ("OneOf" [("List" "Integer") ("List" "Atom")])]))
  =>
  (contains {"A" "Integer"
             "B" "Integer"
             "[B]" "Tuple(Integer)"
             "[A, B]" "Tuple(Integer, Integer)"})))

(facts
 "About Compounds"
 (fact
  "Pass domain down"
  (test-wrapper
   ["foo(A, B)"]
   ("Tuple" ["Ground"]))
  => (contains {"A" "Ground"
                "B" "Ground"})
  (test-wrapper
   ["foo(A, B)"]
   ("Tuple" [("Compound" "foo" ["Integer" "Atom"])]))
  => (contains {"A" "Integer"
                "B" "Atom"})))


(facts
 "About And"
 (fact
  "With Var"
  (test-wrapper
   ["foo(A, B)"]
   ("Tuple" [("And" ["Ground"
                     ("Compound" "foo" ["Var" "Atom"])])]))
  => (contains {"A" "Ground"
                "B" "Atom"})))

(to-spec ("Tuple" [("And" ["Ground"
                           ("Compound" "foo" ["Var" "Atom"])])]))
