(ns prolog-analyzer.transformer-test
  (:require [prolog-analyzer.transformer :as sut]
            [prolog-analyzer.parser :as parser]
            [clojure.test :refer [deftest is are testing]]))


 
(deftest process-rule
  (testing "testing processing of rule"
    (are [y x] (= x (sut/transform y parser/parse))
      "foo(a,X) :- c(X),d."
      {"foo" [{:arity 2
               :arglist [{:term "a", :type :atom}
                         {:term "X", :type :var}]
               :body [{:goal "c"
                       :arity 1
                       :arglist [{:term "X", :type :var}]
                       :module :user}
                      {:goal "d"
                       :arity 0
                       :arglist []
                       :module :user}]}]}
      "foo(a,X) :- a(X). foo(b,X) :- b(X). a(X) :- X = [T], b(T). b(T) :- T = []."
      {"foo" [{:arity 2
               :arglist [{:term "a" :type :atom}
                         {:term "X" :type :var}]
               :body [{:goal "a"
                       :arity 1
                       :arglist [{:term "X" :type :var}]
                       :module :user}]}
              {:arity 2
               :arglist [{:term "b" :type :atom}
                         {:term "X" :type :var}]
               :body [{:goal "b"
                       :arity 1
                       :arglist [{:term "X" :type :var}]
                       :module :user}]}]
       "a" [{:arity 1
             :arglist [{:term "X" :type :var}]
             :body [{:goal :unify-assignment
                     :left {:term "X" :type :var}
                     :right {:term :list :type :list :content [{:term "T" :type :var}]}
                     :module :built-in}
                    {:goal "b"
                     :arity 1
                     :arglist [{:term "T" :type :var}]
                     :module :user}]}]
       "b" [{:arity 1
             :arglist [{:term "T" :type :var}]
             :body [{:goal :unify-assignment
                     :left {:term "T" :type :var}
                     :right {:term :list :type :list}
                     :module :built-in}]}]}
      )))



(deftest transform-test-goals
  (testing "Test, if the different components are correctly transformed to a map"

    (are [x y] (= {"foo" [{:arity 0 :arglist [] :body [y]}]} (sut/transform (str "foo :- " x ".") parser/parse))
      "!" {:goal :Cut
           :arity 0
           :arglist []
           :module :built-in}
      "X is Y+2" {:goal :is-assignment
                  :left {:term "X" :type :var}
                  :right [{:term "Y" :type :var}
                          {:term "2" :type :number}]
                  :module :built-in}
      "X = 3" {:goal :unify-assignment
                :left {:term "X" :type :var}
                :right {:term "3" :type :number}
                :module :built-in}
      "foo(a,b,c)" {:goal "foo"
                    :arity 3
                    :arglist [{:term "a" :type :atom}
                              {:term "b" :type :atom}
                              {:term "c" :type :atom}]
                    :module :user}
      "foo(a,bar(b,c))" {:goal "foo"
                         :arity 2
                         :arglist [{:term "a" :type :atom}
                                   {:term "bar"
                                    :type :compound
                                    :arity 2
                                    :arglist [{:term "b" :type :atom}
                                              {:term "c" :type :atom}]
                                    :infix false}]
                         :module :user}
      "f(A/B/C)" {:goal "f"
                  :arity 1
                  :arglist [{:term "/"
                             :type :compound
                             :arity 2
                             :arglist [{:term "A" :type :var}
                                       {:term "/" :type :compound
                                        :arity 2
                                        :arglist [{:term "B" :type :var}
                                                  {:term "C" :type :var}]
                                        :infix true}]
                             :infix true}]
                  :module :user}

      "f([H|T])" {:goal "f"
                  :arity 1
                  :arglist [{:term :list
                             :type :list
                             :head [{:term "H", :type :var}]
                             :tail {:term "T", :type :var}}]
                  :module :user}
      "f([1,2|T])" {:goal "f"
                    :arity 1
                    :arglist [{:term :list
                               :type :list
                               :head [{:term "1" :type :number}
                                      {:term "2" :type :number}]
                               :tail {:term "T" :type :var}}]
                    :module :user}
      "f([1,2,3])" {:goal "f"
                    :arity 1
                    :arglist [{:term :list
                               :type :list
                               :content [{:term "1" :type :number}
                                         {:term "2" :type :number}
                                         {:term "3" :type :number}]}]
                    :module :user}
      "f([1,2|[3,4]])" {:goal "f"
                        :arity 1
                        :arglist [{:term :list
                                   :type :list
                                   :head [{:term "1" :type :number}
                                          {:term "2" :type :number}]
                                   :tail {:term :list
                                          :type :list
                                          :content [{:term "3" :type :number}
                                                    {:term "4" :type :number}]}}]
                        :module :user}
      "f([])" {:goal "f"
               :arity 1
               :arglist [{:term :list
                          :type :list}]
               :module :user}
      "f([1])" {:goal "f"
                :arity 1
                :arglist [{:term :list
                           :type :list
                           :content [{:term "1" :type :number}]}]
                :module :user}
      "(a,b -> c,d ; e,f)" {:goal :if
                            :cond [{:goal "a" :arity 0 :arglist [] :module :user}
                                   {:goal "b" :arity 0 :arglist [] :module :user}]
                            :then [{:goal "c" :arity 0 :arglist [] :module :user}
                                   {:goal "d" :arity 0 :arglist [] :module :user}]
                            :else [{:goal "e" :arity 0 :arglist [] :module :user}
                                   {:goal "f" :arity 0 :arglist [] :module :user}]
                            :module :built-in})))

(deftest transform-with-brackets
  (testing "Transforming of rule with brackets in the body"
    (are [x y] (= y (sut/transform x parser/parse))
      "foo :- (a,b)." {"foo" [{:arity 0
                               :arglist []
                               :body [{:goal :in-brackets
                                       :body [{:goal "a" :arity 0 :arglist [] :module :user}
                                              {:goal "b" :arity 0 :arglist [] :module :user}]}]}]} )))
(deftest transform-with-semicolons
  (testing "Transforming to clojure structures of rule-bodies with semicolons"
    (are [x y] (= y (sut/transform x parser/parse))
      "foo :- a,b,c." {"foo" [{:arity 0
                               :arglist []
                               :body [{:goal "a" :arity 0 :arglist [] :module :user}
                                      {:goal "b" :arity 0 :arglist [] :module :user}
                                      {:goal "c" :arity 0 :arglist [] :module :user}]}]}
      "foo :- a;b." {"foo" [{:arity 0
                             :arglist []
                             :body [{:goal :or
                                     :arity 2
                                     :arglist [[{:goal "a" :arity 0 :arglist [] :module :user}]
                                               [{:goal "b" :arity 0 :arglist [] :module :user}]]}]}]}
      "foo :- a;b;c." {"foo" [{:arity 0
                               :arglist []
                               :body [{:goal :or
                                       :arity 3
                                       :arglist [[{:goal "a" :arity 0 :arglist [] :module :user}]
                                                 [{:goal "b" :arity 0 :arglist [] :module :user}]
                                                 [{:goal "c" :arity 0 :arglist [] :module :user}]]}]}]}
      "foo :- a,b;c,d." {"foo" [{:arity 0
                                 :arglist []
                                 :body [{:goal :or
                                         :arity 2
                                         :arglist [[{:goal "a" :arity 0 :arglist [] :module :user}
                                                    {:goal "b" :arity 0 :arglist [] :module :user}]
                                                   [{:goal "c" :arity 0 :arglist [] :module :user}
                                                    {:goal "d" :arity 0 :arglist [] :module :user}]]}]}]}
      "foo :- a,(b;c),d." {"foo" [{:arity 0
                                   :arglist []
                                   :body [{:goal "a", :arity 0, :arglist [], :module :user}
                                          {:goal :in-brackets
                                           :body [{:goal :or
                                                   :arity 2
                                                   :arglist [[{:goal "b", :arity 0, :arglist [], :module :user}]
                                                             [{:goal "c", :arity 0, :arglist [], :module :user}]]}]}
                                          {:goal "d", :arity 0, :arglist [], :module :user}]}]}
      )))

 

(deftest transform-to-map-not-clausel
  (testing "Transforming of not"
    (are [x y] (= y (sut/transform x parser/parse))
      "foo :- not(bar(a,b))." {"foo" [{:arity 0
                                       :arglist []
                                       :body [{:goal :not
                                               :body [{:goal "bar"
                                                       :arity 2
                                                       :arglist [{:term "a" :type :atom} {:term "b" :type :atom}]
                                                       :module :user}]}]}]}
      "foo :- \\+ bar(a,b)." {"foo" [{:arity 0
                                     :arglist []
                                     :body [{:goal :not
                                             :body [{:goal "bar"
                                                     :arity 2
                                                     :arglist [{:term "a" :type :atom}
                                                               {:term "b" :type :atom}]
                                                     :module :user}]}]}]})))

(deftest transform-direct-call
  (testing "Transforming of direct calls"
    (are [x y] (= y (sut/transform x parser/parse))
      ":- foo(a,b), bar(c)." {:direct-call [{:body [{:goal "foo" :arity 2 :arglist [{:term "a" :type :atom} {:term "b" :type :atom}] :module :user}
                                                    {:goal "bar" :arity 1 :arglist [{:term "c" :type :atom}] :module :user}]}]})))

