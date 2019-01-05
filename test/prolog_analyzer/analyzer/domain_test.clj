(ns prolog-analyzer.analyzer.domain-test
  (:require [prolog-analyzer.analyzer.domain :as sut]
            [prolog-analyzer.analyzer.pretty-printer :refer [to-string]]
            [clojure.test :refer [deftest is are]]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]

            [clojure.spec.test.alpha :as stest]
            ))

(s/def :simple/arglist (s/and (s/coll-of ::simple-dom) #(< (count %) 4)))
(s/def ::simple-or-spec (s/keys :req-un [:or/spec :simple/arglist]))
(s/def ::simple-and-spec (s/keys :req-un [:and/spec :simple/arglist]))
(s/def ::simple-dom (s/or :normal ::sut/normal-spec
                          :or ::simple-or-spec
                          :and ::simple-and-spec))


(binding [s/*recursion-limit* 2] (gen/generate (s/gen ::simple-dom)))

(deftest to-knf-test
  (are [input expected] (= expected (sut/to-knf input))
    {:spec :a} {:spec :a}
    {:spec :or :arglist [{:spec :a}]} {:spec :a}
    {:spec :and :arglist [{:spec :a}]} {:spec :a}
    {:spec :and :arglist [{:spec :a} {:spec :a}]} {:spec :a}
    {:spec :or :arglist [{:spec :a} {:spec :b} {:spec :a}]} {:spec :or :arglist [{:spec :a} {:spec :b}]}

    ;; (a or b) or (b or c) == a or b or c
    {:spec :or :arglist [{:spec :or :arglist [{:spec :a} {:spec :b}]}
                         {:spec :or :arglist [{:spec :b} {:spec :c}]}]}
    {:spec :or :arglist [{:spec :a} {:spec :b} {:spec :c}]}

    ;; (a or b) and (b or c)
    {:spec :and :arglist [{:spec :or :arglist [{:spec :a} {:spec :b}]}
                         {:spec :or :arglist [{:spec :b} {:spec :c}]}]}
    {:spec :or :arglist [{:spec :and :arglist [{:spec :a} {:spec :b}]}
                         {:spec :and :arglist [{:spec :a} {:spec :c}]}
                         {:spec :b}
                         {:spec :and :arglist [{:spec :b} {:spec :c}]}
                         ]}

    {:spec :and :arglist [{:spec :a} {:spec :b} {:spec :a}]} {:spec :or :arglist [{:spec :and :arglist [{:spec :a} {:spec :b}]}]}
    ))

(deftest knf-test
  (are [x] (sut/knf? (sut/to-knf x))
    {:spec :a}
    {:spec :or :arglist [{:spec :a} {:spec :b}]}
    {:spec :or :arglist [{:spec :and :arglist [{:spec :a} {:spec :b}]} {:spec :c}]}
    ))


(s/fdef to-knf-wrapper
        :args (s/cat :dom ::simple-dom)
        :ret ::sut/dom
        :fn (s/and #(sut/knf? (:ret %))))

(defn to-knf-wrapper [dom]
  (sut/to-knf dom))

(deftest is-subdom-test
  (are [a b] (true? (sut/is-subdom? a b))
    {:spec :integer} {:spec :number}
    {:spec :number} {:spec :any}
    {:spec :float} {:spec :ground}
     {:spec :list :type {:spec :integer}} {:spec :list :type {:spec :atomic}}
    {:spec :tuple :arglist [{:spec :exact :value "hallo"} {:spec :integer}]} {:spec :tuple :arglist [{:spec :atom} {:spec :number}]}
    {:spec :compound :functor "foo" :arglist [{:spec :atom}]} {:spec :compound :functor "foo" :arglist [{:spec :nonvar}]}
    {:spec :number} {:spec :number}
    {:spec :exact :value "empty"} {:spec :exact :value "empty"}
    {:spec :exact :value "empty"} {:spec :atomic}
    {:spec :any :name "X"} {:spec :any})
  (are [a b] (false? (sut/is-subdom? a b))
    {:spec :any} {:spec :any :name "X"}
    {:spec :any :name "X"} {:spec :any :name "Y"}
    {:spec :atom} {:spec :exact :value "p"}
    {:spec :exact :value "a"} {:spec :exact :value "b"}
    {:spec :list :type {:spec :number}} {:spec :list :type {:spec :integer}}
    {:spec :tuple :arglist [{:spec :integer}]} {:spec :tuple :arglist [{:spec :integer} {:spec :atom}]}
    {:spec :compound :functor "foo" :arglist [{:spec :integer}]} {:spec :compound :functor "foo2" :arglist [{:spec :integer}]}))

(deftest merge-dom-test
  (are [dom1 dom2 result] (= result (sut/merge-dom dom1 dom2))
    {:spec :integer} {:spec :integer} {:spec :integer}
    {:spec :integer} {:spec :number} {:spec :integer}
    {:spec :exact :value "empty"} {:spec :ground} {:spec :exact :value "empty"}
    {:spec :exact :value "empty"} {:spec :exact :value "nan"} nil))


(defn- ->ht-list [head & tail]
  (if (nil? tail)
    {:type :list :head head :tail {:type :atomic :term "[]"}}
    {:type :list :head head :tail (apply ->ht-list tail)}))

(defn ->lstsp [type]
  {:spec :list :type type})

(defn ->int [value]
  {:type :integer :value value})

(deftest get-initial-dom-non-trivial
  (are [arg spec env] (= env (reduce-kv #(assoc %1 (to-string %2) %3) {} (sut/get-initial-dom-from-spec arg spec)))

    ; empty-list
    {:type :atomic :term "[]"},   {:spec :list :type {:spec :number}},   {"[]" [{:spec :list :type {:spec :number}}]}
    {:type :atomic :term "[]"},   {:spec :tuple :arglist []},            {"[]" [{:spec :tuple :arglist []}]}
    {:type :atomic :term "[]"},   {:spec :ground},                       {"[]" [{:spec :list :type {:spec :ground}}]}
    {:type :atomic :term "[]"},   {:spec :nonvar},                       {"[]" [{:spec :list :type {:spec :any}}]}
    {:type :atomic :term "[]"},   {:spec :any},                          {"[]" [{:spec :list :type {:spec :any}}]}

    ; list - list
    (->ht-list {:type :integer :value 2} {:type :integer :value 3}), {:spec :list :type {:spec :number}},
    {2,       [{:spec :integer}]
     "[3]",   [{:spec :list :type {:spec :number}}]
     3,       [{:spec :integer}]
     "[2, 3]", [{:spec :list :type {:spec :number}}]
     }

    {:type :list :head {:type :var :name "X"} :tail {:type :var :name "T"}} {:spec :list :type {:spec :integer}}
    {"X" [{:spec :integer}]
     "T" [{:spec :list :type {:spec :integer}}]
     "[X|T]" [{:spec :list :type {:spec :integer}}]}

    ; list - tuple
    (->ht-list {:type :var :name "X"} {:type :atom :term "hallo"}), {:spec :tuple :arglist [{:spec :integer} {:spec :atom}]}
    {"[X, hallo]"    [{:spec :tuple :arglist [{:spec :integer} {:spec :atom}]}]
     "X"             [{:spec :integer}]
     "[hallo]"       [{:spec :tuple :arglist [{:spec :atom}]}]
     "hallo"         [{:spec :atom}]}

    ; list - ground
    {:type :list :head {:type :var :name "X"} :tail {:type :var :name "T"}}, {:spec :ground}
    {"[X|T]",  [{:spec :list :type {:spec :ground}}]
     "X",      [{:spec :ground}]
     "T",      [{:spec :list :type {:spec :ground}}]}

    {:type :list :head {:type :integer :value 2} :tail {:type :var :name "T"}}, {:spec :ground}
    {"[2|T]",  [{:spec :list :type {:spec :ground}}]
     2,        [{:spec :integer}]
     "T",      [{:spec :list :type {:spec :ground}}]}

    ; list - nonvar
    {:type :list :head {:type :var :name "X"} :tail {:type :var :name "T"}}, {:spec :nonvar}
    {"[X|T]",  [{:spec :list :type {:spec :nonvar}}]
     "X",      [{:spec :any}]
     "T",      [{:spec :list :type {:spec :nonvar}}]}

    {:type :list :head {:type :integer :value 2} :tail {:type :var :name "T"}}, {:spec :nonvar}
    {"[2|T]",  [{:spec :list :type {:spec :nonvar}}]
     2,        [{:spec :integer}]
     "T",      [{:spec :list :type {:spec :nonvar}}]}

    ; list - any
    {:type :list :head {:type :var :name "X"} :tail {:type :var :name "T"}}, {:spec :any}
    {"[X|T]",  [{:spec :list :type {:spec :any}}]
     "X",      [{:spec :any}]
     "T",      [{:spec :list :type {:spec :any}}]}

    {:type :list :head {:type :integer :value 2} :tail {:type :var :name "T"}}, {:spec :any}
    {"[2|T]",  [{:spec :list :type {:spec :any}}]
     2,        [{:spec :integer}]
     "T",      [{:spec :list :type {:spec :any}}]}


                                        ; compound - compound
                                        ; compound - ground
                                        ; compound - nonvar
                                        ; compound - any

                                        ; var - list
                                        ; var - number
                                        ; var - atomic

                                        ; integer - number
    ))


                                        ; TODO: add cases where it should broke
                                        ; list - number
                                        ; compound - list
                                        ; compound - different functors
                                        ; list - tuple wrong args
                                        ; compound - atom
                                        ; integer - atom
