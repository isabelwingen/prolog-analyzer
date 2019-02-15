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

(deftest fill-env-for-term-with-spec-test-any
  (are [in out]
      (= [out] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env in {:spec :any}) in))
    {:type :any :term "anyanyany"} {:spec :any}
    {:type :ground :term "anyanyany"} {:spec :ground}
    {:type :nonvar :term "anyanyany"} {:spec :nonvar}
    {:type :atom :term "ize"} {:spec :atom}
    {:type :atomic :term "cake"} {:spec :atomic}
    {:type :integer :value 42} {:spec :integer}
    {:type :number :value 23} {:spec :number}
    {:type :float :value 3.1415} {:spec :float}
    {:type :list :head {:type :integer :value 11235813} :tail {:type :atomic :term "[]"}} {:spec :list :type {:spec :any}}
    {:type :compound :functor "wrap" :arglist [{:type :atom :term "salad"} {:type :atom :term "tomatoes"}]} {:spec :compound :functor "wrap" :arglist [{:spec :any} {:spec :any}]}
    {:type :var :name "X"} {:spec :var} ;;TODO: any or var?
    {:type :anon_var :name "_1603"} {:spec :var};; TODO: any or var?
    ))


(deftest fill-env-for-term-with-spec-test-ground
  (are [in out]
      (= [out] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env in {:spec :ground}) in))
    {:type :any :term "anyanyany"} {:spec :ground}
    {:type :ground :term "anyanyany"} {:spec :ground}
    {:type :nonvar :term "anyanyany"} {:spec :ground}
    {:type :atom :term "bunker"} {:spec :atom}
    {:type :atomic :term "cake"} {:spec :atomic}
    {:type :integer :value 42} {:spec :integer}
    {:type :number :value 23} {:spec :number}
    {:type :float :value 3.1415} {:spec :float}
    {:type :list :head {:type :integer :value 11235813} :tail {:type :atomic :term "[]"}} {:spec :list :type {:spec :ground}}
    {:type :compound :functor "wrap" :arglist [{:type :atom :term "salad"} {:type :atom :term "tomatoes"}]} {:spec :compound :functor "wrap" :arglist [{:spec :ground} {:spec :ground}]}
    {:type :var :name "X"} {:spec :ground}
    {:type :anon_var :name "_1603"} {:spec :ground}
    ))

(deftest fill-env-for-term-with-spec-test-nonvar
  (are [in out]
      (= [out] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env in {:spec :nonvar}) in))
    {:type :any :term "anyanyany"} {:spec :nonvar}
    {:type :ground :term "anyanyany"} {:spec :ground}
    {:type :nonvar :term "anyanyany"} {:spec :nonvar}
    {:type :atom :term "cake"} {:spec :atom}
    {:type :atomic :term "cake"} {:spec :atomic}
    {:type :integer :value 42} {:spec :integer}
    {:type :number :value 23} {:spec :number}
    {:type :float :value 3.1415} {:spec :float}
    {:type :list :head {:type :integer :value 11235813} :tail {:type :atomic :term "[]"}} {:spec :list :type {:spec :any}}
    {:type :compound :functor "wrap" :arglist [{:type :atom :term "salad"} {:type :atom :term "tomatoes"}]} {:spec :compound :functor "wrap" :arglist [{:spec :any} {:spec :any}]}
    {:type :var :name "X"} {:spec :nonvar}
    {:type :anon_var :name "_1603"} {:spec :nonvar}
    ))

(deftest fill-env-for-term-with-spec-test-var
  (are [in out]
      (= [out] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env in {:spec :var}) in))
    {:type :ground :term "anyanyany"} (sut/ALREADY-NONVAR)
    {:type :nonvar :term "nonononvar"} (sut/ALREADY-NONVAR)
    {:type :atom :term "batman"} (sut/ALREADY-NONVAR)
    {:type :atomic :term "cake"} (sut/ALREADY-NONVAR)
    {:type :integer :value 42} (sut/ALREADY-NONVAR)
    {:type :number :value 23} (sut/ALREADY-NONVAR)
    {:type :float :value 3.1415} (sut/ALREADY-NONVAR)
    {:type :list :head {:type :integer :value 11235813} :tail {:type :atomic :term "[]"}} (sut/ALREADY-NONVAR)
    {:type :compound :functor "wrap" :arglist [{:type :atom :term "salad"} {:type :atom :term "tomatoes"}]} (sut/ALREADY-NONVAR)
    )
  (is (utils/valid-env? (sut/fill-env-for-term-with-spec test-env {:type :var :name "X"} {:spec :var})))
  (is (utils/valid-env? (sut/fill-env-for-term-with-spec (sut/add-doms-to-node test-env {:type :var :name "X"} {:spec :any}) {:type :var :name "X"} {:spec :var})))
  (is (utils/valid-env? (sut/fill-env-for-term-with-spec (sut/add-doms-to-node test-env {:type :var :name "X"} {:spec :var} {:spec :var} {:spec :any}) {:type :var :name "X"} {:spec :var})))
  (is ((complement utils/valid-env?) (sut/fill-env-for-term-with-spec (sut/add-doms-to-node test-env {:type :var :name "X"} {:spec :ground}) {:type :var :name "X"} {:spec :var})))
  (is (utils/valid-env? (sut/fill-env-for-term-with-spec test-env {:type :anon_var :name "X"} {:spec :var})))
  (is (utils/valid-env? (sut/fill-env-for-term-with-spec (sut/add-doms-to-node test-env {:type :anon_var :name "X"} {:spec :any}) {:type :anon_var :name "X"} {:spec :var})))
  (is (utils/valid-env? (sut/fill-env-for-term-with-spec (sut/add-doms-to-node test-env {:type :anon_var :name "X"} {:spec :var} {:spec :var} {:spec :any}) {:type :anon_var :name "X"} {:spec :var})))
  (is ((complement utils/valid-env?) (sut/fill-env-for-term-with-spec (sut/add-doms-to-node test-env {:type :anon_var :name "X"} {:spec :ground}) {:type :anon_var :name "X"} {:spec :var}))))


(deftest fill-env-for-term-with-spec-test-atomic
  (are [term]
      (= [{:spec :atomic}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term {:spec :atomic}) term))
    {:type :atomic :term "cake"}
    {:type :atomic :term "1603"}
    {:type :var :name "X"}
    {:type :anon_var :name "_0410"}
    )
  (is (= [{:spec :atom}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env {:type :atom :term "cake"} {:spec :atomic}) {:type :atom :term "cake"})))
  (is (= [{:spec :number}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env {:type :number :value 2} {:spec :atomic}) {:type :number :value 2})))
  (is (= [{:spec :integer}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env {:type :integer :value 2} {:spec :atomic}) {:type :integer :value 2})))
  (is (= [{:spec :float}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env {:type :float :value 2.0} {:spec :atomic}) {:type :float :value 2.0})))
  (are [term]
      (= [(sut/WRONG-TYPE term {:spec :atomic})] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term {:spec :atomic}) term))
    {:type :list :head {:type :integer :value 2} :tail {:type :atomic :term "[]"}}
    {:type :compound :functor "foo" :arglist [{:spec :atom :term "foo"}]}
    )
  )

(deftest fill-env-for-term-with-spec-test-atom
  (are [term]
      (= [{:spec :atom}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term {:spec :atom}) term))
    {:type :atom :term "cake"}
    {:type :atomic :term "cake"}
    {:type :var :name "X"}
    {:type :anon_var :name "_0410"}
    )
  (are [term]
      (= [(sut/WRONG-TYPE term {:spec :atom})] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term {:spec :atom}) term))
    {:type :atomic :term "[]"}
    {:type :atomic :term "1603"}
    {:type :list :head {:type :integer :value 2} :tail {:type :atomic :term "[]"}}
    {:type :compound :functor "foo" :arglist [{:spec :atom :term "foo"}]}
    )
  )

(deftest fill-env-for-term-with-spec-test-exact
  (are [term]
      (= [{:spec :exact :value "cake"}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term {:spec :exact :value "cake"}) term))
    {:type :atomic :term "cake"}
    {:type :atom :term "cake"}
    {:type :var :name "X"}
    {:type :anon_var :name "_0410"}
    )
  (are [term]
        (= [(sut/WRONG-TYPE term {:spec :exact :value "cake"})] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term {:spec :exact :value "cake"}) term))
   {:type :list :head {:type :integer :value 2} :tail {:type :atomic :term "[]"}}
   {:type :compound :functor "foo" :arglist [{:spec :atom :term "foo"}]}
   {:type :atom :term "nocake"}
   {:type :atomic :term "1"}
   {:type :integer :value 2}
   {:type :number :value 2}
   {:type :float :value 2.0}
   )
  )

(deftest fill-env-for-term-with-spec-test-number
  (are [term]
      (= [{:spec :number}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term {:spec :number}) term))
    {:type :atomic :term "3.14"}
    {:type :atomic :term "100"}
    {:type :var :name "X"}
    {:type :anon_var :name "_234"})
  (is (= [{:spec :integer}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env {:type :integer :value 2} {:spec :number}) {:type :integer :value 2})))
  (is (= [{:spec :float}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env {:type :float :value 2.5} {:spec :number}) {:type :float :value 2.5})))
  (are [term]
      (= [(sut/WRONG-TYPE term {:spec :number})] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term {:spec :number}) term))
    {:type :atomic :term "no"}
    {:type :atom}
    {:type :exact :term "no"}
    {:type :list :head {:type :number :value 0} :tail {:type :atomic :term "[]"}}
    {:type :compound :functor "node" :arglist [{:type :atom :term "hello"}]}
    )
  )


(deftest fill-env-for-term-with-spec-test-integer
  (are [term]
      (= [{:spec :integer}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term {:spec :integer}) term))
    {:type :integer :value 42}
    {:type :number :value 42}
    {:type :atomic :term "42"}
    {:type :var :name "X"}
    {:type :anon_var :name "_234"})

  (are [term]
      (= [(sut/WRONG-TYPE term {:spec :integer})] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term {:spec :integer}) term))
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
      (= [{:spec :float}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term {:spec :float}) term))
    {:type :float :value 3.14}
    {:type :number :value 1.0}
    {:type :atomic :term "3.14"}
    {:type :var :name "X"}
    {:type :anon_var :name "_234"})

  (are [term]
      (= [(sut/WRONG-TYPE term {:spec :float})] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term {:spec :float}) term))
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

(deftest fill-env-for-term-with-spec-test-list
  (are [term]
      (= [{:spec :list :type {:spec :integer}}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term {:spec :list :type {:spec :integer}}) term))
    {:type :atomic :term "[]"} ;; TODO: do we want `atomic` as an additional type?
    {:type :list :head {:type :integer :value 2} :tail {:type :list :head {:type :integer :value 3} :tail {:type :atomic :term "[]"}}}
    {:type :var :name "X"}
    {:type :anon_var :name "_0410"}
    )
  (are [term]
      (false? (utils/valid-env? (sut/fill-env-for-term-with-spec test-env term {:spec :list :type {:spec :integer}})))
    {:type :list :head {:type :float :value 2.5} :tail {:type :atomic :term "[]"}} ;TODO: atm the error is only visible in the HEAD term. Should it be at top level?
    {:type :compound :functor "foo" :arglist [{:spec :atom :term "foo"}]}
    {:type :atom :term "nocake"}
    {:type :atomic :term "cake"}
    {:type :atomic :term "1"}
    {:type :integer :value 2}
    {:type :number :value 2}
    {:type :float :value 2.0}))

(deftest fill-env-for-term-with-spec-test-tuple
  (are [term]
      (= [{:spec :tuple :arglist [{:spec :integer} {:spec :atom}]}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term {:spec :tuple :arglist [{:spec :integer} {:spec :atom}]}) term))
    {:type :list :head {:type :integer :value 2} :tail {:type :list :head {:type :atomic :term "cake"} :tail {:type :atomic :term "[]"}}}
    {:type :list :head {:type :number :value 2} :tail {:type :list :head {:type :atom :term "cake"} :tail {:type :atomic :term "[]"}}}
    {:type :var :name "X"}
    {:type :anon_var :name "_0410"}
    )
  (are [term]
      (false? (utils/valid-env? (sut/fill-env-for-term-with-spec test-env term {:spec :tuple :arglist [{:spec :integer} {:spec :atom}]})))
    {:type :atomic :term "[]"}
    {:type :list :head {:type :float :value 2.5} :tail {:type :atomic :term "[]"}} ;TODO: atm the error is only visible in the HEAD term. Should it be at top level?
    {:type :compound :functor "foo" :arglist [{:spec :atom :term "foo"}]}
    {:type :atom :term "nocake"}
    {:type :atomic :term "cake"}
    {:type :atomic :term "1"}
    {:type :integer :value 2}
    {:type :number :value 2}
    {:type :float :value 2.0}))

(deftest fill-env-for-term-with-spec-test-compound
  (are [term]
      (= [{:spec :compound :functor "foo" :arglist [{:spec :integer} {:spec :atom}]}] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term {:spec :compound :functor "foo" :arglist [{:spec :integer} {:spec :atom}]}) term))
    {:type :compound :functor "foo" :arglist [{:type :integer :value 2} {:type :atomic :term "cake"}]}
    {:type :compound :functor "foo" :arglist [{:type :number :value 2} {:type :atom :term "cake"}]}
    {:type :var :name "X"}
    {:type :anon_var :name "_0410"}
    )
  (are [term]
      (false? (utils/valid-env? (sut/fill-env-for-term-with-spec test-env term {:spec :compound :functor "fooy" :arglist [{:spec :integer} {:spec :atom}]})))
    {:type :atomic :term "[]"}
    {:type :list :head {:type :float :value 2.5} :tail {:type :atomic :term "[]"}} ;TODO: atm the error is only visible in the HEAD term. Should it be at top level?
    {:type :compound :functor "foo" :arglist [{:spec :atom :term "foo"}]}
    {:type :compound :functor "not-foo" :arglist [{:type :integer :value 2} {:type :atomic :term "cake"}]}
    {:type :compound :functor "foo" :arglist [{:type :float :value 2.5} {:type :atomic :term "cake"}]}
    {:type :atom :term "nocake"}
    {:type :atomic :term "cake"}
    {:type :atomic :term "1"}
    {:type :integer :value 2}
    {:type :number :value 2}
    {:type :float :value 2.0}))


(deftest fill-env-for-term-with-spec-test-specvar
  (let [term {:type :integer :value 42}
        specvar {:spec :specvar :name 0}
        expected-env (-> test-env
                         (sut/add-doms-to-node term specvar {:spec :integer})
                         (sut/add-doms-to-node specvar {:spec :integer}))]
    (is (= expected-env (sut/fill-env-for-term-with-spec test-env term specvar))))
  (let [term {:type :atom :term "nevegonnagiveyouup"}
        specvar {:spec :specvar :name 0}
        expected-env (-> test-env
                      (sut/add-doms-to-node term specvar {:spec :atom})
                      (sut/add-doms-to-node specvar {:spec :atom}))]
    (is (= expected-env (sut/fill-env-for-term-with-spec test-env term specvar))))
  (let [term {:type :list :head {:type :integer :value 1} :tail {:type :atomic :term "[]"}}
        specvar {:spec :specvar :name 0}
        spec-to-be-filled {:spec :list :type specvar}
        expected-env (-> test-env
                         (sut/add-doms-to-node term spec-to-be-filled)
                         (sut/add-doms-to-node {:type :integer :value 1} specvar {:spec :integer})
                         (sut/add-doms-to-node specvar {:spec :integer})
                         )
        actual-env (sut/fill-env-for-term-with-spec test-env term spec-to-be-filled)]
    (is (= expected-env actual-env))))

(deftest fill-env-for-term-with-spec-test-one-of
  (are [term spec expected-env] (= expected-env (sut/fill-env-for-term-with-spec test-env term spec))
    {:type :atom :term "hallohallo"}
    {:spec :one-of :arglist [{:spec :integer} {:spec :atomic}]}
    (sut/add-doms-to-node test-env {:type :atom :term "hallohallo"} {:spec :atom})

    {:type :atom :term "hallohallo"}
    {:spec :one-of :arglist [{:spec :atom} {:spec :atomic}]}
    (sut/add-doms-to-node test-env {:type :atom :term "hallohallo"} {:spec :atom})

    {:type :integer :value 3}
    {:spec :one-of :arglist [{:spec :number} {:spec :integer} {:spec :atom}]}
    (sut/add-doms-to-node test-env {:type :integer :value 3} {:spec :integer})

    {:type :integer :value 3}
    {:spec :one-of :arglist [{:spec :integer} {:spec :specvar :name 0}]}
    (sut/add-doms-to-node test-env {:type :integer :value 3} {:spec :one-of :arglist [{:spec :integer} {:spec :and :arglist [{:spec :specvar :name 0} {:spec :integer}]}]})

    {:type :list :head {:type :integer :value 1} :tail {:type :atomic :term "[]"}}
    {:spec :one-of :arglist [{:spec :list :type {:spec :number}} {:spec :tuple :arglist [{:spec :atomic}]}]}
    (sut/add-doms-to-node test-env
                          {:type :list :head {:type :integer :value 1} :tail {:type :atomic :term "[]"}}
                          {:spec :one-of :arglist [{:spec :list :type {:spec :number}} {:spec :tuple :arglist [{:spec :atomic}]}]})
    ))

(deftest fill-env-for-term-with-spec-test-and
  (are [term spec expected-env] (= expected-env (sut/fill-env-for-term-with-spec test-env term spec))
    {:type :atom :term "hallohallo"}
    {:spec :and :arglist [{:spec :atom} {:spec :atomic}]}
    (sut/add-doms-to-node test-env {:type :atom :term "hallohallo"} {:spec :atom})

    {:type :atom :term "hallohallo"}
    {:spec :and :arglist [{:spec :any} {:spec :atomic}]}
    (sut/add-doms-to-node test-env {:type :atom :term "hallohallo"} {:spec :atom})

    {:type :integer :value 3}
    {:spec :and :arglist [{:spec :ground} {:spec :specvar :name 0}]}
    (-> test-env
        (sut/add-doms-to-node {:spec :specvar :name 0} {:spec :integer})
        (sut/add-doms-to-node {:type :integer :value 3} {:spec :integer} {:spec :specvar :name 0}))

    ))

(deftest fill-env-for-term-with-spec-test-user-defined
  (are [term spec expected-env] (= expected-env (sut/fill-env-for-term-with-spec test-env term spec))
    {:type :atom :term "empty"}
    {:spec :user-defined :name "tree" :arglist [{:spec :integer}]}
    (sut/add-doms-to-node test-env {:type :atom :term "empty"} {:spec :user-defined :name "tree" :arglist [{:spec :integer}]} {:spec :exact :value "empty"})

    {:type :atom :term "empty"}
    {:spec :user-defined :name "tree" :arglist [{:spec :specvar :name 0}]}
    (-> test-env
        (sut/add-doms-to-node {:type :atom :term "empty"} {:spec :user-defined :name "tree" :arglist [{:spec :specvar :name 0}]} {:spec :exact :value "empty"}))


    ))
