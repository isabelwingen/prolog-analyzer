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

(deftest fill-env-for-term-with-spec-test
  (are [term spec expected-dom]
    (= expected-dom (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term spec) term))

    {:type :integer :value 1} {:spec :any} [{:spec :integer}]
    {:type :integer :value 1} {:spec :ground} [{:spec :integer}]
    {:type :integer :value 1} {:spec :nonvar} [{:spec :integer}]
    {:type :integer :value 1} {:spec :atomic} [{:spec :integer}]
    {:type :integer :value 1} {:spec :number} [{:spec :integer}]
    {:type :integer :value 1} {:spec :integer} [{:spec :integer}]
    {:type :integer :value 1} {:spec :user-defined :name "atomOrInt"} [{:spec :user-defined :name "atomOrInt"} {:spec :integer}]
    {:type :integer :value 1} {:spec :specvar :name "X"} [{:spec :specvar :name "X"}]
    {:type :integer :value 1} {:spec :one-of :arglist [{:spec :integer} {:spec :exact :value "empty"}]} [{:spec :integer}]
    {:type :integer :value 1} {:spec :and :arglist [{:spec :integer}]} [{:spec :integer}]

    {:type :atomic :term "not-a-list"} {:spec :any} [{:spec :atomic}]
    {:type :atomic :term "not-a-list"} {:spec :ground} [{:spec :atomic}]
    {:type :atomic :term "not-a-list"} {:spec :nonvar} [{:spec :atomic}]
    {:type :atomic :term "not-a-list"} {:spec :atomic} [{:spec :atomic}]
    {:type :atomic :term "not-a-list"} {:spec :atom} [{:spec :atom}]
    {:type :atomic :term "not-a-list"} {:spec :one-of :arglist [{:spec :exact :value "not-a-list"} {:spec :atom}]} [{:spec :atomic} {:spec :one-of :arglist [{:spec :exact :value "not-a-list"} {:spec :atom}]}]
    {:type :atomic :term "not-a-list"} {:spec :and :arglist [{:spec :atom}]} [{:spec :atom}]
    {:type :atomic :term "not-a-list"} {:spec :user-defined :name "atomOrInt"} [{:spec :user-defined :name "atomOrInt"} {:spec :atom}]
    )

  (are [term spec]
      (false? (utils/valid-env? (sut/fill-env-for-term-with-spec test-env term spec)))
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



    ))
