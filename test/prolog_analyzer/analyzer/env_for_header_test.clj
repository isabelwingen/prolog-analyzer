(ns prolog-analyzer.analyzer.env-for-header-test
  (:require [prolog-analyzer.analyzer.env-for-header :as sut]
            [prolog-analyzer.test-helper :refer [to-term to-spec] :as th]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [prolog-analyzer.utils :as utils]
            [midje.sweet :refer :all]))

(defmacro test-wrapper [header-vars spec]
  `(utils/env->map (sut/get-env {} {:arglist (apply vector (map to-term ~header-vars))} (to-spec ~spec))))

(facts
 (fact
  "Pass domain of head upwards"
  (test-wrapper
   ["H" "[H|T]"]
   ("Tuple" ["Integer" ("OneOf" [("List" ("OneOf" ["Integer" "Atom"]))
                                 ("List" "Float")])]))
  =>
  (contains {"H" ["Integer"]
             "T" ["List(OneOf(Integer, Atom))"]
             "[H|T]" ["List(OneOf(Integer, Atom))"]}))
 (fact
  "Pass domain of head upwards and pass it down to tail"
  (test-wrapper
   ["A" "[A, B]"]
   ("Tuple" ["Integer" ("OneOf" [("Tuple" ["Integer" "Atom"])
                                 ("Tuple" ["Float" "Float"])])]))
  =>
  (contains {"A" ["Integer"]
             "B" ["Atom"]}))
 (fact
  "Pass domain of tail upwards"
  (test-wrapper
   ["A" "[B]" "[A, B]"]
   ("Tuple" ["Float" ("Tuple" ["Integer"]) "Any"]))
  =>
  (contains {"A" ["Float"]
             "B" ["Integer"]
             "[B]" ["Tuple(Integer)"]
             "[A, B]" ["Tuple(Float, Integer)"]})
  (test-wrapper
   ["[B]" "[A, B]"]
   ("Tuple"
    [("Tuple" ["Integer"])
     ("OneOf" [("List" "Integer") ("List" "Atom")])]))
  =>
  (contains {"A" ["Integer"]
             "B" ["Integer"]
             "[B]" ["Tuple(Integer)"]
             "[A, B]" ["Tuple(Integer, Integer)"]})



  ))

(map to-term ["[B]" "[A, B]"])
(to-spec ("Tuple" [("Tuple" ["Integer"]) ("OneOf" [("List" "Integer") ("List" "Atom")])]))
