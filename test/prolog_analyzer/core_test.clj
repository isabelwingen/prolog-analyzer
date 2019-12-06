(ns prolog-analyzer.core-test
  (:require [prolog-analyzer.core :as sut]
            [clojure.java.io :as io]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [midje.sweet :refer :all]))

;; Test Build Ins

(def BUILT-IN "edns/builtins.edn")
(def TMP "resources/tmp.pl")
(def PREAMBLE ":- module(tmp,[]).\n\n")

(defn analyze-snippet [snippet]
  (spit (io/file TMP) (str PREAMBLE snippet))
  (sut/run TMP))



(defn transform-to-types [data [_ _ arity :as pred-id]]
  (let [res (->> (get-in data [:post-specs pred-id])
                 (map :conclusion)
                 (map (partial map (partial sort-by :id)))
                 (map (partial map (partial map :type)))
                 (map (partial apply map vector))
                 (map (partial map set))
                 (map (partial map r/->OneOfSpec))
                 (map (partial map ru/simplify))
                 (apply map vector)
                 (map set)
                 (map r/->AndSpec)
                 (map ru/simplify)
                 (map r/to-string)
                 (interleave (range arity))
                 (apply hash-map)
                 )
        ]
    (Thread/sleep 5000)
    res))

(facts
 "Simple types"
 (fact "Atom"
       (-> "foo(X) :- atom(X)."
           analyze-snippet
           (transform-to-types ["tmp" "foo" 1]))
       => (contains {0 "Atom"}))
 (fact "Atomic"
       (-> "foo(X) :- atomic(X)."
           analyze-snippet
           (transform-to-types ["tmp" "foo" 1]))
       => (contains {0 "Atomic"})
       )
 (fact "Callable"
       (-> "foo(X) :- callable(X)."
           analyze-snippet
           (transform-to-types ["tmp" "foo" 1]))
       => (contains {0 "OneOf(Atom, Compound)"})
       )
 (fact "Compound"
       (-> "foo(X) :- compound(X)."
           analyze-snippet
           (transform-to-types ["tmp" "foo" 1]))
       => (contains {0 "Compound"})
       )

 (fact "Float"
       (-> "foo(X) :- float(X)."
           analyze-snippet
           (transform-to-types ["tmp" "foo" 1]))
       => (contains {0 "Float"})
       )

 (fact "Ground"
       (-> "foo(X) :- ground(X)."
           analyze-snippet
           (transform-to-types ["tmp" "foo" 1]))
       => (contains {0 "Ground"})
       )

 (fact "Integer"
       (-> "foo(X) :- integer(X)."
           analyze-snippet
           (transform-to-types ["tmp" "foo" 1]))
       => (contains {0 "Integer"})
       )

 (fact "Number"
       (-> "foo(X) :- number(X)."
           analyze-snippet
           (transform-to-types ["tmp" "foo" 1]))
       => (contains {0 "Number"})
       )
 (fact "Nonvar"
       (-> "foo(X) :- nonvar(X)."
           analyze-snippet
           (transform-to-types ["tmp" "foo" 1]))
       => (contains {0 "Nonvar"})
       )

 (fact "Var"
       (-> "foo(X) :- var(X)."
           analyze-snippet
           (transform-to-types ["tmp" "foo" 1]))
       => (contains {0 "Var"})
       )
 )
