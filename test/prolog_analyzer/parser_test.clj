(ns prolog-analyzer.parser-test
  (:require [prolog-analyzer.parser :as sut]
            [prolog-analyzer.records :as r]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))

(defn- first-parse-result [l]
  (second l))

(def preamble
  ":- module(tmp,[]).\n:- use_module(prolog_analyzer,[enable_write_out/0,declare_spec/1,define_spec/2,spec_pre/2,spec_post/3,spec_invariant/2]).\n:- enable_write_out.\n\n")


(defn test-helper [code]
  (spit "prolog/tmp.pl" (str preamble code))
  (let [res (sut/read-prolog-code-as-raw-edn "prolog/prolog_analyzer.pl" "prolog/tmp.pl")]
    (io/delete-file "prolog/tmp.pl")
    res))

(test-helper "foo(a,b).")

(deftest parse-facts
  (are [x y] (= {:type :pred :content y} (first-parse-result (test-helper x)))
    "foo(a,b)."
    {:name "foo"
     :module "tmp"
     :arity 2
     :arglist [{:term "a", :type :atom} {:term "b", :type :atom}]
     :body [{:goal "true", :module "self", :arity 0, :arglist []}]
     }

    "write_out."
    {:name "write_out"
     :module "tmp"
     :arity 0
     :arglist []
     :body [{:goal "true", :module "self", :arity 0, :arglist []}]
     }))


(deftest parse-list
  (are [x y] (= {:type :pred :content {:name "foo"  :module "tmp" :arity 1 :arglist [y] :body [{:goal "true" :module "self" :arity 0 :arglist []}]}}
                (first-parse-result (test-helper (str "foo(" x ")."))))
    "[1,2,3]"
    {:type :list
     :head {:value 1 :type :integer}
     :tail {:type :list
            :head {:value 2 :type :integer}
            :tail {:type :list
                   :head {:value 3 :type :integer}
                   :tail {:type :empty-list}}}}
    "[H|T]"
    {:type :list
     :head {:name "H" :type :var}
     :tail {:name "T" :type :var}}
    "[]"
    {:type :empty-list}
    "[1]"
    {:type :list
     :head {:value 1 :type :integer}
     :tail {:type :empty-list}}
    "[1,2|T]"
    {:type :list
     :head {:value 1 :type :integer}
     :tail {:type :list
            :head {:value 2 :type :integer}
            :tail {:name "T" :type :var}}}
    ))

(deftest parse-compounds
  (are [x y] (= {:type :pred :content {:name "foo" :module "tmp" :arity 1 :arglist [y] :body [{:goal "true" :module "self" :arity 0 :arglist []}]}}
                (first-parse-result (test-helper (str "foo(" x ")."))))

    "2/3"
    {:type :compound
     :functor "/"
     :arglist [{:value 2 :type :integer} {:value 3 :type :integer}]}
    "2/3/4"
    {:type :compound
     :functor "/"
     :arglist [{:type :compound
                :functor "/"
                :arglist [{:value 2 :type :integer} {:value 3 :type :integer}]}
               {:value 4
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
   (= {:type :pred
       :content {:name "foo"
                 :module "tmp"
                 :arity 0
                 :arglist []
                 :body [{:goal ":not"
                         :module "self"
                         :arity 1
                         :arglist [{:type :compound
                                    :functor "bar"
                                    :arglist [{:term "a" :type :atom} {:term "b" :type :atom}]}]}]}}
      (first-parse-result (test-helper "foo :- \\+ bar(a,b)."))
      )))


(deftest parse-rules
  (are [x y] (= {:type :pred :content y} (first-parse-result (test-helper x)))
    "foo(a,b) :- bar(a,b)."
    {:name "foo"
     :module "tmp"
     :arity 2
     :arglist [{:term "a", :type :atom} {:term "b", :type :atom}]
     :body [{:goal "bar"
             :module "self"
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
             :module "self"
             :arity 0
             :arglist []}
            {:goal "bar"
             :module "self"
             :arity 2
             :arglist [{:term "a" :type :atom}
                       {:name "X" :type :var}]}
            {:goal "c"
             :module "self"
             :arity 1
             :arglist [{:term "b" :type :atom}]}]}
    ))


(deftest parse-or
  (are [x y] (= {:type :pred :content {:name "foo"
                                       :module "tmp"
                                       :arity 0
                                       :arglist []
                                       :body y}}
                (first-parse-result (test-helper (str "foo :- " x "."))))
    "a;b"
    [{:goal :or
      :arity 2
      :arglist [[{:goal "a" :module "self" :arity 0 :arglist []}] [{:goal "b" :module "self" :arity 0 :arglist []}]]}]
    "a,b;c,d;e,f"
    [{:goal :or
      :arity 3
      :arglist [[{:goal "a" :module "self" :arity 0 :arglist []} {:goal "b" :module "self" :arity 0 :arglist []}]
                [{:goal "c" :module "self" :arity 0 :arglist []} {:goal "d" :module "self" :arity 0 :arglist []}]
                [{:goal "e" :module "self" :arity 0 :arglist []} {:goal "f" :module "self" :arity 0 :arglist []}]]}]
    "a,(b;c),d"
    [{:goal "a" :module "self" :arity 0 :arglist []}
     {:goal :or :arity 2 :arglist [[{:goal "b" :module "self" :arity 0 :arglist []}]
                                                  [{:goal "c" :module "self" :arity 0 :arglist []}]]}
     {:goal "d" :module "self" :arity 0 :arglist []}]
    ))

(deftest parse-if
  (are [x y] (= {:type :pred :content {:name "foo"
                                       :module "tmp"
                                       :arity 0
                                       :arglist []
                                       :body y}}
                (first-parse-result (test-helper (str "foo :- " x "."))))
    "(a -> b;c)"
    [{:goal :or
      :arity 2
      :arglist [[{:goal :if
                  :arity 2
                  :arglist [[{:goal "a" :module "self" :arity 0 :arglist []}]
                            [{:goal "b" :module "self" :arity 0 :arglist []}]]}]
                [{:goal "c" :module "self" :arity 0 :arglist []}]]}]
    "(a -> b)"
    [{:goal :if
      :arity 2
      :arglist [[{:goal "a" :module "self" :arity 0 :arglist []}]
                [{:goal "b" :module "self" :arity 0 :arglist []}]]}]
    "a;b,c -> d,e;f"
    [{:goal :or
      :arity 3
      :arglist [[{:goal "a" :module "self" :arity 0 :arglist []}]
                [{:goal :if
                  :arity 2
                  :arglist [[{:goal "b" :module "self" :arity 0 :arglist []} {:goal "c" :module "self" :arity 0 :arglist []}]
                            [{:goal "d" :module "self" :arity 0 :arglist []} {:goal "e" :module "self" :arity 0 :arglist []}]]}]
                [{:goal "f" :module "self" :arity 0 :arglist []}]]}]
    ))

(deftest parse-specs
  (are [input type output] (= {:type type :content output}
                              (first-parse-result (test-helper input)))
    ":- declare_spec(skip)."
    :declare_spec
    {:goal "declare_spec", :module "self" :arity 1, :arglist [{:term "skip" :type :atom}]}

    ":- define_spec(skip,atom(skip))."
    :define_spec
    {:goal "define_spec", :module "self" :arity 2, :arglist [{:term "skip" :type :atom}
                                                             {:type :compound
                                                              :functor "atom"
                                                              :arglist [{:term "skip" :type :atom}]}]}

    ":- spec_pre(foo/2,[int,int])."
    :spec_pre
    {:goal "spec_pre", :module "self" :arity 2, :arglist [{:type :compound
                                                           :functor ":",
                                                           :arglist
                                                           [{:term "tmp" :type :atom}
                                                            {:type :compound
                                                             :functor "/"
                                                             :arglist [{:term "foo" :type :atom}
                                                                       {:value 2 :type :integer}]}]}
                                                          {:type :list
                                                           :head {:term "int" :type :atom}
                                                           :tail {:type :list
                                                                  :head {:term "int" :type :atom}
                                                                  :tail {:type :empty-list}}}]}

    ":- spec_post(foo/2,[any,int],[int,int])."
    :spec_post
    {:goal "spec_post", :module "self", :arity 3, :arglist [{:type :compound
                                                             :functor ":"
                                                             :arglist
                                                             [{:term "tmp" :type :atom}
                                                              {:type :compound
                                                               :functor "/"
                                                               :arglist [{:term "foo" :type :atom}
                                                                         {:value 2 :type :integer}]}]}
                                                            {:type :list
                                                             :head {:term "any" :type :atom}
                                                             :tail {:type :list
                                                                    :head {:term "int" :type :atom}
                                                                    :tail {:type :empty-list}}}
                                                            {:type :list
                                                             :head {:term "int" :type :atom}
                                                             :tail {:type :list
                                                                    :head {:term "int" :type :atom}
                                                                    :tail {:type :empty-list}}}]}

    ":- spec_invariant(foo/1,[int])."
    :spec_inv
    {:goal "spec_invariant" :module "self"  :arity 2 :arglist [{:type :compound
                                                                :functor ":"
                                                                :arglist
                                                                [{:term "tmp" :type :atom}
                                                                 {:type :compound
                                                                  :functor "/"
                                                                  :arglist [{:term "foo" :type :atom}
                                                                            {:value 1 :type :integer}]}]}
                                                               {:type :list
                                                                :head {:term "int" :type :atom}
                                                                :tail {:type :empty-list}}]}
    ))




(deftest process-files:preds
  (let [result (sut/process-prolog-file "prolog/prolog_analyzer.pl" "resources/spec-test.pl")]
    (is (coll? (get-in result [:preds "spec_test" "member_int" 2])))))
