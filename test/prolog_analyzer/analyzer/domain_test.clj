(ns prolog-analyzer.analyzer.domain-test
  (:require [prolog-analyzer.analyzer.domain :as sut]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.utils :as utils]
            [ubergraph.core :as uber]
            [prolog-analyzer.test-helper :refer [to-term to-spec]]
            [midje.sweet :refer :all]))


(defmacro test-wrapper [term spec]
  `(utils/env->map (sut/add-to-dom (uber/digraph) (to-term ~term) (to-spec ~spec))))

(facts
 (fact
  (test-wrapper
   "[A, B]"
   ("Tuple" ["Integer" "Atom"]))
  =>
  (contains {"[A, B]"    ["List(Any)" "Tuple(Integer, Atom)"]
             "[B]"       ["List(Any)" "Tuple(Atom)"]
             "A"         ["Integer" "Any"]
             "B"         ["Any" "Atom"]
             "[]"        ["Tuple()" "List(Any)"]}))
 (fact
  (test-wrapper
   "[]"
   ("Tuple" []))
  =>
  (contains {"[]" ["Tuple()" "EmptyList"]}))
 (fact
  (test-wrapper
   "[]"
   ("Tuple" ["Integer"]))
  =>
  (contains {"[]" ["Tuple(Integer)" "EmptyList"]})))
