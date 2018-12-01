(ns prolog-analyzer.parser-test
  (:require [prolog-analyzer.parser :as sut]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))

(deftest foo
  (is (= 1 1)))

 (def preamble
  ":- module(tmp,[]).\n:- use_module(prolog_analyzer,[enable_write_out/0]).\n:- enable_write_out.\n\n")

(defn- first-parse-result [l]
  (second l))

(defn test-helper [code]
  (spit "prolog/tmp.pl" (str preamble code))
  (let [res (sut/read-prolog-code-as-raw-edn "prolog/tmp.pl")]
    (io/delete-file "prolog/tmp.pl")
    res))

(deftest parse-facts
  (are [x y] (= {:type :pred :content y} (first-parse-result (test-helper x)))
    "foo(a,b)."
    {:name "foo"
     :module "tmp"
     :arity 2
     :arglist [{:term "a", :type :atom} {:term "b", :type :atom}]
     :body [{:goal "true", :arity 0, :arglist []}]
     }

    "write_out."
    {:name "write_out"
     :module "tmp"
     :arity 0
     :arglist []
     :body [{:goal "true", :arity 0, :arglist []}]
     }))


(deftest parse-list
  (are [x y] (= {:type :pred :content {:name "foo"  :module "tmp" :arity 1 :arglist [y] :body [{:goal "true" :arity 0 :arglist []}]}}
                (first-parse-result (test-helper (str "foo(" x ")."))))
    "[1,2,3]"
    {:type :list
     :arglist [{:value 1 :type :integer} {:value 2 :type :integer} {:value 3 :type :integer}]}
    "[H|T]"
    {:type :head-tail-list
     :head {:name "H" :type :var}
     :tail {:name "T" :type :var}}
    "[]"
    {:term "[]"
     :type :atomic}
    "[1]"
    {:type :list
     :arglist [{:value 1 :type :integer}]}
    "[1,2|T]"
    {:type :head-tail-list
     :head {:value 1 :type :integer}
     :tail {:type :head-tail-list
            :head {:value 2 :type :integer}
            :tail {:name "T" :type :var}}}
    ))

(deftest parse-compounds
  (are [x y] (= {:type :pred :content {:name "foo" :module "tmp" :arity 1 :arglist [y] :body [{:goal "true" :arity 0 :arglist []}]}}
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
                       {:name "X" :type :var}]}
            {:goal "c"
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
      :arglist [[{:goal "a" :arity 0 :arglist []}] [{:goal "b" :arity 0 :arglist []}]]}]
    "a,b;c,d;e,f"
    [{:goal :or
      :arity 3
      :arglist [[{:goal "a" :arity 0 :arglist []} {:goal "b" :arity 0 :arglist []}]
                [{:goal "c" :arity 0 :arglist []} {:goal "d" :arity 0 :arglist []}]
                [{:goal "e" :arity 0 :arglist []} {:goal "f" :arity 0 :arglist []}]]}]
    "a,(b;c),d"
    [{:goal "a" :arity 0 :arglist []}
     {:goal :or :arity 2 :arglist [[{:goal "b" :arity 0 :arglist []}]
                                   [{:goal "c" :arity 0 :arglist []}]]}
     {:goal "d" :arity 0 :arglist []}]
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
                  :arglist [[{:goal "a" :arity 0 :arglist []}]
                            [{:goal "b" :arity 0 :arglist []}]]}]
                [{:goal "c" :arity 0 :arglist []}]]}]
    "(a -> b)"
    [{:goal :if
      :arity 2
      :arglist [[{:goal "a" :arity 0 :arglist []}]
                [{:goal "b" :arity 0 :arglist []}]]}]
    "a;b,c -> d,e;f"
    [{:goal :or
      :arity 3
      :arglist [[{:goal "a" :arity 0 :arglist []}]
                [{:goal :if
                  :arity 2
                  :arglist [[{:goal "b" :arity 0 :arglist []} {:goal "c" :arity 0 :arglist []}]
                            [{:goal "d" :arity 0 :arglist []} {:goal "e" :arity 0 :arglist []}]]}]
                [{:goal "f" :arity 0 :arglist []}]]}]
    ))


(deftest parse-specs
  (are [input type output] (= {:type type :content output}
                              (first-parse-result (test-helper input)))
    ":- declare_spec(skip)."
    :declare_spec
    {:goal "declare_spec", :arity 1, :arglist [{:term "skip" :type :atom}]}

    ":- define_spec(skip,atom(skip))."
    :define_spec
    {:goal "define_spec", :arity 2, :arglist [{:term "skip" :type :atom}
                                              {:type :compound
                                               :functor "atom"
                                               :arglist [{:term "skip" :type :atom}]}]}

    ":- spec_pre(foo/2,[int,int])."
    :spec_pre
    {:goal "spec_pre", :arity 2, :arglist [{:type :compound
                                            :functor "/"
                                            :arglist [{:term "foo" :type :atom}
                                                      {:value 2 :type :integer}]}
                                           {:type :list
                                            :arglist [{:term "int" :type :atom}
                                                      {:term "int" :type :atom}]}]}

    ":- spec_post(foo/2,[any,int],[int,int])."
    :spec_post
    {:goal "spec_post", :arity 3, :arglist [{:type :compound
                                             :functor "/"
                                             :arglist [{:term "foo" :type :atom}
                                                       {:value 2 :type :integer}]}
                                            {:type :list
                                             :arglist [{:term "any" :type :atom}
                                                       {:term "int" :type :atom}]}
                                            {:type :list
                                             :arglist [{:term "int" :type :atom}
                                                       {:term "int" :type :atom}]}]}

    ":- spec_invariant(foo/1,[int])."
    :spec_inv
    {:goal "spec_invariant" :arity 2 :arglist [{:type :compound :functor "/"
                                          :arglist [{:term "foo" :type :atom}
                                                    {:value 1 :type :integer}]}
                                         {:type :list
                                          :arglist [{:term "int" :type :atom}]}]}

    ))


(deftest process-files:spec_def
  (let [result (sut/process-prolog-file "resources/process-file-test.pl")]
    (is (= {{:spec "foo"} {:spec :compound :functor "foo" :arglist [{:spec "int"} {:spec "int"}]}
            {:spec "intOrVar"} {:spec :one_of :arglist [{:spec "int"} {:spec :var}]}}
           (:specs result)))
    (is
     (= {"member_int"
         {2 [[{:spec "int"} {:spec :list :type {:spec "int"}}]
             [{:spec "var"} {:spec :list :type {:spec "int"}}]]}
         "foo"
         {3 [[{:spec "foo"} {:spec "intOrVar"} {:spec "intOrVar"}]]}}
        (:spec_pre result)))
    (is
     (= {"member_int" {2 [[[{:spec "var"} {:spec :list :type {:spec "int"}}] [{:spec "int"} {:spec :list :type {:spec "int"}}]]]}
         "foo" {3 [[[{:spec "foo"} {:spec "intOrVar"} {:spec "intOrVar"}] [{:spec "foo"} {:spec "int"} {:spec "int"}]]
                   [[{:spec "nonvar"} {:spec "int"} {:spec "int"}] [{:spec "foo"} {:spec "int"} {:spec "int"}]]]}}
        (:spec_post result)))
    (is
     (= {"member_int" {2 [[{:spec "any"} {:spec "ground"}]]}}))
    )
  )

(deftest process-files:preds
  (let [result (sut/process-prolog-file "resources/process-file-test.pl")]
    (is (and (coll? (get-in result [:pred "process_file_test" "member_int" 2]))))))
