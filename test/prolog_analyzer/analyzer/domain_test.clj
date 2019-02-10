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
                                     {:spec :one-of, :arglist [{:spec :integer} {:spec :atom}]}}}])))
(def spec-simple {:spec :user-defined :name "atomOrInt"})
(def spec-tree-int {:spec :user-defined :name "tree" :arglist [{:spec :integer}]})
(def spec-tree-x {:spec :user-defined :name "tree" :arglist [{:spec :specvar :name 0}]})
