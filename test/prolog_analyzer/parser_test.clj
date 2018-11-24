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
    {:type :list
     :arglist [{:term 1 :type :integer} {:term 2 :type :integer} {:term 3 :type :integer}]}
    "[H|T]"
    {:type :head-tail-list
     :head {:term "H" :type :var}
     :tail {:term "T" :type :var}}
    "[]"
    {:term "[]"
     :type :atomic}
    "[1]"
    {:type :list
     :arglist [{:term 1 :type :integer}]}
    "[1,2|T]"
    {:type :head-tail-list
     :head {:term 1 :type :integer}
     :tail {:type :head-tail-list
            :head {:term 2 :type :integer}
            :tail {:term "T" :type :var}}}
    ))

(deftest parse-compounds
  (are [x y] (= {:type :fact :content {:goal "foo" :arity 1 :arglist [y]}}
                (first (test-helper (str "foo(" x ")."))))

    "2/3"
    {:type :compound
     :functor "/"
     :arglist [{:term 2 :type :integer} {:term 3 :type :integer}]}
    "2/3/4"
    {:type :compound
     :functor "/"
     :arglist [{:type :compound
                :functor "/"
                :arglist [{:term 2 :type :integer} {:term 3 :type :integer}]}
               {:term 4
                :type :integer}]}
    "foo(a,bar(c,d))"
    {:type :compound
     :functor "foo"
     :arglist [{:term "a" :type :atom}
               {:type :compound
                :functor "bar"
                :arglist [{:term "c" :type :atom} {:term "d" :type :atom}]}]}
    ))


(deftest parse-not
  (is
   (= {:type :rule
       :content {:name "foo"
                 :module "tmp"
                 :arity 0
                 :arglist []
                 :body [{:goal ":not"
                         :arity 1
                         :arglist [{:type :compound
                                    :functor "bar"
                                    :arglist [{:term "a" :type :atom} {:term "b" :type :atom}]}]}]}}
      (first (test-helper "foo :- \\+ bar(a,b)."))
      )))


(deftest parse-rules
  (are [x y] (= {:type :rule :content y} (first (test-helper x)))
    "foo(a,b) :- bar(a,b)."
    {:name "foo"
     :module "tmp"
     :arity 2
     :arglist [{:term "a", :type :atom} {:term "b", :type :atom}]
     :body [{:goal "bar"
             :arity 2
             :arglist [{:term "a" :type :atom} {:term "b" :type :atom}]}]}
    "foo(a/b) :- !, bar(a,X), c(b)." 
    {:name "foo"
     :module "tmp"
     :arity 1
     :arglist [{:type :compound
                :functor "/"
                :arglist [{:term "a" :type :atom} {:term "b" :type :atom}]}]
     :body [{:goal "!"
             :arity 0
             :arglist []}
            {:goal "bar"
             :arity 2
             :arglist [{:term "a" :type :atom}
                       {:term "X" :type :var}]}
            {:goal "c"
             :arity 1
             :arglist [{:term "b" :type :atom}]}]}
    ))



(test-helper "foo(a/b) :- 1, bar(a,X), c(b).")

(seq {:a 1 :b [{:c :d} {:e :f}]})
