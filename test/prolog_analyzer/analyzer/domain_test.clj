(ns prolog-analyzer.analyzer.domain-test
  (:require [prolog-analyzer.analyzer.domain :as sut]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.utils :as utils]
            [ubergraph.core :as uber]
            [prolog-analyzer.test-helper :refer [to-term to-spec]]
            [midje.sweet :refer :all]))


(defmacro test-wrapper [term spec]
  `(utils/env->map (sut/add-to-dom (uber/digraph) true (to-term ~term) (to-spec ~spec))))

(facts
 (fact
  (test-wrapper
   "[A, B]"
   ("Tuple" ["Integer" "Atom"]))
  =>
  (contains {"[A, B]"    "Tuple(Integer, Atom)"
             "[B]"       "Tuple(Atom)"
             "A"         "Integer"
             "B"         "Atom"
             "[]"        "EmptyList"}))
 (fact
  (test-wrapper
   "[]"
   ("Tuple" []))
  =>
  (contains {"[]" "EmptyList"}))
 (fact
  (test-wrapper
   "[]"
   ("Tuple" ["Integer"]))
  =>
  (contains {"[]" "ERROR: No valid intersection of EmptyList and Tuple(Integer)"})))
