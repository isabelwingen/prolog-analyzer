(ns prolog-analyzer.test-builtins
  (:require  [midje.sweet :refer :all]
             [prolog-analyzer.core :as core]
             [clojure.java.io :as io]
             [prolog-analyzer.record-utils :as ru]
             [prolog-analyzer.records :as r]))

(def BUILT-IN "edns/builtins.edn")
(def TMP "resources/tmp.pl")
(def PREAMBLE ":- module(tmp,[]).\n\n")

(defn analyze-snippet [snippet]
  (spit (io/file TMP) (str PREAMBLE snippet))
  (core/run TMP))

(def q (analyze-snippet "foo(X,Y) :- atom(X)."))

(defn any-postspec? [typings]
  (every? (comp  :type) typings))

(->> (get-in q [:post-specs ["tmp" "foo" 2]])
     (map :conclusion))

(defn transform-to-types [data [_ _ arity :as pred-id]]
  (->> (get-in data [:post-specs pred-id])
       last
       :conclusion
       (map (partial sort-by :id))
       (map (partial map :type))
       (apply map vector)
       (map set)
       (map r/->OneOfSpec)
       (map ru/simplify)
       (map r/to-string)
       (interleave (range arity))
       (apply hash-map)))

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
