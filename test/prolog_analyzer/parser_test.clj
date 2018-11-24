(ns prolog-analyzer.parser-test
  (:require [prolog-analyzer.parser :as sut]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))

(deftest foo
  (is (= 1 1)))

 (def preamble
  ":- module(tmp,[]).\n:- use_module(term_expander,[enable_write_out/0]).\n:- enable_write_out.\n\n")

(defn test-helper [code]
  (spit "prolog/tmp.pl" (str preamble code))
  (let [res (sut/read-prolog-code "prolog/tmp.pl")]
    (io/delete-file "prolog/tmp.pl")
    res))

(deftest parse-facts
  (are [x y] (= {:type :fact :content y} (first (test-helper x)))
    "foo(a,b)."
    {:goal "foo"
     :arity 2
     :arglist [{:term "a", :type :atom} {:term "b", :type :atom}]}

    "write_out."
    {:goal "write_out"
     :arity 0
     :arglist []}))


(deftest parse-list
  (are [x y] (= {:type :fact :content {:goal "foo" :arity 1 :arglist [y]}}
                (first (test-helper (str "foo(" x ")."))))
    "[1,2,3]"
    {:term "[1,2,3]"
     :type :list
     :arglist [{:term 1 :type :integer} {:term 2 :type :integer} {:term 3 :type :integer}]}
    "[H|T]"
    {:term "[_2938|_2940]"
     :type :list
     :head {:term "_2938" :type :var}
     :tail {:term "_2940" :type :var}}
    "[]"
    {:term "[]"
     :type :atomic}
    "[1]"
    {:term "[1]"
     :type :list
     :arglist [{:term 1 :type :integer}]}
    "[1,2|T]"
    {:term "[1,2|_2958]"
     :type :list
     :head {:term 1 :type :integer}
     :tail {:term "[2|_2958]"
            :type :list
            :head {:term 2 :type :integer}
            :tail {:term "_2958" :type :var}}}
    ))



(test-helper "member([1,2|T]).")

(seq {:a 1 :b [{:c :d} {:e :f}]})
