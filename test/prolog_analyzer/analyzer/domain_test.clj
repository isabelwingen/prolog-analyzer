(ns prolog-analyzer.analyzer.domain-test
  (:require [prolog-analyzer.analyzer.domain :as sut]
            [prolog-analyzer.utils :as utils]
            [prolog-analyzer.analyzer.pretty-printer :refer [to-string]]
            [clojure.test :refer [deftest is are]]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [ubergraph.core :as uber]
            [loom.graph]
            [ubergraph.protocols]
            [clojure.spec.test.alpha :as stest]
            ))

(deftest add-doms-to-node-test
  (is (= [:a :b :c]
         (-> (uber/digraph)
             (uber/add-nodes-with-attrs [:x {:dom [:a]}])
             (sut/add-doms-to-node :x :b :c)
             (uber/attr :x :dom))))
  (is (= [:a :b]
         (-> (uber/digraph)
             (sut/add-doms-to-node :x :a :b)
             (uber/attr :x :dom)))))


(def test-env (-> (uber/digraph) (uber/add-nodes-with-attrs
                                  [:ENVIRONMENT
                                   {:user-defined-specs
                                    {{:spec :user-defined, :name "tree", :arglist [{:spec :specvar, :name "X"}]}
                                     {:spec :one-of,
                                      :arglist
                                      [{:spec :compound,
                                        :functor "node",
                                        :arglist
                                        [{:spec :user-defined,
                                          :name "tree",
                                          :arglist [{:spec :specvar, :name "X"}]}
                                         {:spec :specvar, :name "X"}
                                         {:spec :user-defined,
                                          :name "tree",
                                          :arglist [{:spec :specvar, :name "X"}]}]}
                                       {:spec :exact, :value "empty"}]}

                                     {:spec :user-defined :name "atomOrInt"}
                                     {:spec :one-of, :arglist [{:spec :integer} {:spec :atom}]}

                                     {:spec :user-defined :name "blob"}
                                     {:spec :exact :value "blob"}}}])))
(def spec-simple {:spec :user-defined :name "atomOrInt"})
(def spec-tree-int {:spec :user-defined :name "tree" :arglist [{:spec :integer}]})
(def spec-tree-x {:spec :user-defined :name "tree" :arglist [{:spec :specvar :name 0}]})

(def unfolded-tree-int {:spec :one-of :arglist [{:spec :compound :functor "node" :arglist [spec-tree-int {:spec :integer} spec-tree-int]} {:spec :exact :value "empty"}]})

(deftest fill-env-for-term-with-spec-test-integer
  (are [term]
      (= [{:spec :integer}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec2 test-env term {:spec :integer}) term))
    {:type :integer :value 42}
    {:type :number :value 42}
    {:type :atomic :term "42"}
    {:type :var :name "X"}
    {:type :anon_var :name "_234"})

  (are [term]
      (= [(sut/WRONG-TYPE term {:spec :integer})] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec2 test-env term {:spec :integer}) term))
    {:type :number :value 3.14}
    {:type :float :value 3.14}
    {:type :atomic :term "3.14"}
    {:type :atomic :term "no"}
    {:type :atom}
    {:type :exact :term "no"}
    {:type :list :head {:type :integer :value 0} :tail {:type :atomic :term "[]"}}
    {:type :compound :functor "node" :arglist [{:type :atom :term "hello"}]}
    )
  )

(deftest fill-env-for-term-with-spec-test-float
  (are [term]
      (= [{:spec :float}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec2 test-env term {:spec :float}) term))
    {:type :float :value 3.14}
    {:type :number :value 1.0}
    {:type :atomic :term "3.14"}
    {:type :var :name "X"}
    {:type :anon_var :name "_234"})

  (are [term]
      (= [(sut/WRONG-TYPE term {:spec :float})] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec2 test-env term {:spec :float}) term))
    {:type :number :value 42}
    {:type :integer :value 123}
    {:type :atomic :term "1"}
    {:type :atomic :term "no"}
    {:type :atom}
    {:type :exact :term "no"}
    {:type :list :head {:type :float :value 0} :tail {:type :atomic :term "[]"}}
    {:type :compound :functor "node" :arglist [{:type :atom :term "hello"}]}
    )
  )

(deftest fill-env-for-term-with-spec-test-number
  (are [term]
      (= [{:spec :number}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec2 test-env term {:spec :number}) term))
    {:type :atomic :term "3.14"}
    {:type :atomic :term "100"}
    {:type :var :name "X"}
    {:type :anon_var :name "_234"})
  (is (= [{:spec :integer}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec2 test-env {:type :integer :value 2} {:spec :number}) {:type :integer :value 2})))
  (is (= [{:spec :float}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec2 test-env {:type :float :value 2.5} {:spec :number}) {:type :float :value 2.5})))
  (are [term]
      (= [(sut/WRONG-TYPE term {:spec :number})] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec2 test-env term {:spec :number}) term))
    {:type :atomic :term "no"}
    {:type :atom}
    {:type :exact :term "no"}
    {:type :list :head {:type :number :value 0} :tail {:type :atomic :term "[]"}}
    {:type :compound :functor "node" :arglist [{:type :atom :term "hello"}]}
    )
  )

(deftest fill-env-for-term-with-spec-test-atom
  (are [term]
      (= [{:spec :atom}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec2 test-env term {:spec :atom}) term))
    {:type :atom :term "cake"}
    {:type :atomic :term "cake"}
    {:type :var :name "X"}
    {:type :anon_var :name "_0410"}
    )
  (are [term]
      (= [(sut/WRONG-TYPE term {:spec :atom})] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec2 test-env term {:spec :atom}) term))
    {:type :atomic :term "[]"}
    {:type :atomic :term "1603"}
    {:type :list :head {:type :integer :value 2} :tail {:type :atomic :term "[]"}}
    {:type :compound :functor "foo" :arglist [{:spec :atom :term "foo"}]}
    )
  )

(deftest fill-env-for-term-with-spec-test-atomic
  (are [term]
      (= [{:spec :atomic}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec2 test-env term {:spec :atomic}) term))
    {:type :atomic :term "cake"}
    {:type :atomic :term "[]"}
    {:type :atomic :term "1603"}
    {:type :var :name "X"}
    {:type :anon_var :name "_0410"}
    )
  (is (= [{:spec :atom}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec2 test-env {:type :atom :term "cake"} {:spec :atomic}) {:type :atom :term "cake"})))
  (is (= [{:spec :number}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec2 test-env {:type :number :value 2} {:spec :atomic}) {:type :number :value 2})))
  (is (= [{:spec :integer}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec2 test-env {:type :integer :value 2} {:spec :atomic}) {:type :integer :value 2})))
  (is (= [{:spec :float}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec2 test-env {:type :float :value 2.0} {:spec :atomic}) {:type :float :value 2.0})))
  (are [term]
      (= [(sut/WRONG-TYPE term {:spec :atomic})] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec2 test-env term {:spec :atomic}) term))
    {:type :list :head {:type :integer :value 2} :tail {:type :atomic :term "[]"}}
    {:type :compound :functor "foo" :arglist [{:spec :atom :term "foo"}]}
    )
  )

(deftest fill-env-for-term-with-spec-test-exact
  (are [term]
      (= [{:spec :exact :value "cake"}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec2 test-env term {:spec :exact :value "cake"}) term))
    {:type :atomic :term "cake"}
    {:type :atom :term "cake"}
    {:type :var :name "X"}
    {:type :anon_var :name "_0410"}
    )
  (are [term]
      (= [(sut/WRONG-TYPE term {:spec :exact :value "cake"})] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec2 test-env term {:spec :exact :value "cake"}) term))
    {:type :list :head {:type :integer :value 2} :tail {:type :atomic :term "[]"}}
    {:type :compound :functor "foo" :arglist [{:spec :atom :term "foo"}]}
    {:type :atom :term "nocake"}
    {:type :atomic :term "1"}
    {:type :integer :value 2}
    {:type :number :value 2}
    {:type :float :value 2.0}
    )
  )





(comment (deftest fill-env-for-term-with-spec-test
           (are [term spec expected-dom]
               (= (vector term spec expected-dom) (vector term spec (utils/get-dom-of-term (sut/fill-env-for-term-with-spec2 test-env term spec) term)))

             {:type :integer :value 1} {:spec :any} [{:spec :integer}]
             {:type :integer :value 1} {:spec :ground} [{:spec :integer}]
             {:type :integer :value 1} {:spec :nonvar} [{:spec :integer}]
             {:type :integer :value 1} {:spec :atomic} [{:spec :integer}]
             {:type :integer :value 1} {:spec :number} [{:spec :integer}]
             {:type :integer :value 1} {:spec :user-defined :name "atomOrInt"} [{:spec :user-defined :name "atomOrInt"} {:spec :integer}]
             {:type :integer :value 1} {:spec :specvar :name "X"} [{:spec :specvar :name "X"} {:spec :integer}]
             {:type :integer :value 1} {:spec :one-of :arglist [{:spec :integer} {:spec :exact :value "empty"}]} [{:spec :integer}]
             {:type :integer :value 1} {:spec :and :arglist [{:spec :integer}]} [{:spec :integer}]

             {:type :atom :term "abc"} {:spec :any} [{:spec :atom}]
             {:type :atom :term "abc"} {:spec :ground} [{:spec :atom}]
             {:type :atom :term "abc"} {:spec :nonvar} [{:spec :atom}]
             {:type :atom :term "abc"} {:spec :atomic} [{:spec :atom}]
             {:type :atom :term "abc"} {:spec :atom} [{:spec :atom}]
             {:type :atom :term "abc"} {:spec :user-defined :name "atomOrInt"} [{:spec :user-defined :name "atomOrInt"} {:spec :atom}]
             {:type :atom :term "abc"} {:spec :specvar :name "X"} [{:spec :specvar :name "X"} {:spec :atom}]
             {:type :atom :term "abc"} {:spec :one-of :arglist [{:spec :atom} {:spec :exact :value "empty"}]} [{:spec :atom}]
             {:type :atom :term "abc"} {:spec :and :arglist [{:spec :atom}]} [{:spec :atom}]


             {:type :atomic :term "not-a-list"} {:spec :any} [{:spec :atomic}]
             {:type :atomic :term "not-a-list"} {:spec :ground} [{:spec :atomic}]
             {:type :atomic :term "not-a-list"} {:spec :nonvar} [{:spec :atomic}]
             {:type :atomic :term "not-a-list"} {:spec :atomic} [{:spec :atomic}]
             {:type :atomic :term "not-a-list"} {:spec :atom} [{:spec :atom}]
             {:type :atomic :term "not-a-list"} {:spec :one-of :arglist [{:spec :exact :value "not-a-list"} {:spec :atom}]} [{:spec :atomic} {:spec :one-of :arglist [{:spec :exact :value "not-a-list"} {:spec :atom}]}]
             {:type :atomic :term "not-a-list"} {:spec :and :arglist [{:spec :atom}]} [{:spec :atom}]
             {:type :atomic :term "not-a-list"} {:spec :user-defined :name "atomOrInt"} [{:spec :user-defined :name "atomOrInt"} {:spec :atom}]

             {:type :var :name "X"} {:spec :any} [{:spec :any}]
             {:type :var :name "X"} {:spec :ground} [{:spec :ground}]
             {:type :var :name "X"} {:spec :nonvar} [{:spec :nonvar}]
             {:type :var :name "X"} {:spec :var} [{:spec :var}]
             {:type :var :name "X"} {:spec :atomic} [{:spec :atomic}]
             {:type :var :name "X"} {:spec :atom} [{:spec :atom}]
             {:type :var :name "X"} {:spec :number} [{:spec :number}]
             {:type :var :name "X"} {:spec :integer} [{:spec :integer}]
             {:type :var :name "X"} {:spec :float} [{:spec :float}]
             {:type :var :name "X"} {:spec :list :type {:spec :integer}} [{:spec :list :type {:spec :integer}}]
             {:type :var :name "X"} {:spec :tuple :arglist [{:spec :atom}]} [{:spec :tuple :arglist [{:spec :atom}]}]
             {:type :var :name "X"} {:spec :compound :arglist [{:spec :number}]} [{:spec :compound ::arglist [{:spec :number}]}]
             {:type :var :name "X"} spec-tree-int [spec-tree-int unfolded-tree-int]
             {:type :var :name "X"} {:spec :one-of :arglist [{:spec :number} {:spec :atom}]} [{:spec :one-of :arglist [{:spec :number} {:spec :atom}]}]
             {:type :var :name "X"} {:spec :and :arglist [{:spec :list :type {:spec :integer}} {:spec :user-defined :name "tree" :arglist [{:spec :number}]}]} [{:spec :any}])

           (are [term spec]
               (false? (utils/valid-env? (sut/fill-env-for-term-with-spec2 test-env term spec)))
             {:type :integer :value 1} {:spec :var}
             {:type :integer :value 1} {:spec :anon_var}
             {:type :integer :value 1} {:spec :atom}
             {:type :integer :value 1} {:spec :exact :value "no"}
             {:type :integer :value 1} {:spec :float}
             {:type :integer :value 1} {:spec :list :type {:spec :integer}}
             {:type :integer :value 1} {:spec :compound :arglist [{:spec :atom}]}
             {:type :integer :value 1} {:spec :tuple :arglist [{:spec :atom}]}
             {:type :integer :value 1} {:spec :user-defined :name "blob"}


             {:type :atomic :term "not-a-list"} {:spec :list :type {:spec :integer}}
             {:type :atomic :term "not-a-list"} {:spec :compound :arglist [{:spec :atom}]}
             {:type :atomic :term "not-a-list"} {:spec :tuple :arglist [{:spec :atom}]}
             {:type :atomic :term "not-a-list"} {:spec :user-defined :name "blob"}
                                        ; TODO: This atomic is clearly not a number. Do we check it?



             )))
