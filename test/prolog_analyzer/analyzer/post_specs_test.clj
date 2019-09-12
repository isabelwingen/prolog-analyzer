(ns prolog-analyzer.analyzer.post-specs-test
  (:require [prolog-analyzer.analyzer.post-specs :as sut]
            [ubergraph.core :as uber]
            [prolog-analyzer.records :as r]
            [midje.sweet :refer :all]))




(defn register-post-specs [args post-specs]
  (uber/attr
   (sut/register-post-specs (uber/digraph) args post-specs)
   :environment
   :post-specs))

(facts
 "About regist-post-specs"
 (fact
  "Empty guard"
  (register-post-specs [:a :b :c] [{:guard [] :conclusion [[{:id 0 :type :x} {:id 1 :type :y} {:id 2 :type :z}]]}])
  =>
  (contains [{:guard [] :conclusion [[{:arg :a :type :x} {:arg :b :type :y} {:arg :c :type :z}]]}]))
 (fact
  (register-post-specs [:a :b :c] [{:guard [{:id 0 :type :x} {:id 1 :type :y}] :conclusion [[{:id 2 :type :z}], [{:id 2 :type :x}]]}
                                   {:guard [{:id 2 :type :p}] :conclusion [[{:id 0 :type :q} {:id 1 :type :q}]]}])


  => (contains [{:conclusion [[{:arg :c :type :z}], [{:arg :c :type :x}]] :guard [{:arg :a :type :x} {:arg :b :type :y}]}
                {:conclusion [[{:arg :a :type :q} {:arg :b :type :q}]] :guard [{:arg :c :type :p}]}])))


(defn test-wrapper [arglist values post-specs]
  (let [env (reduce (fn [e [n d]] (uber/add-nodes-with-attrs e [n {:dom d}])) (uber/digraph) (map vector arglist values))]
    (reduce
     (fn [e [n d]] (assoc e (r/to-string n) (r/to-string d)))
     {}
     (-> env
         (sut/register-post-specs arglist post-specs)
         sut/get-next-steps-from-post-specs))))


(facts
 "apply-post-specs"
 (fact
  (test-wrapper
   [(r/->VarTerm "X") (r/->VarTerm "Y") (r/->VarTerm "Z")]
   [(r/->AtomSpec) (r/->AnySpec) (r/->AnySpec)]
   [{:guard [{:id 0 :type (r/->AtomSpec)}] :conclusion [[{:id 1 :type (r/->FloatSpec)}], [{:id 1 :type (r/->IntegerSpec)} {:id 2 :type (r/->AtomSpec)}]]}])
  => (contains {"[Z, Y]" "OneOf(Tuple(Any, Float), Tuple(Atom, Integer))"}))
 (fact
  (test-wrapper
   [(r/->VarTerm "X") (r/->VarTerm "Y")]
   [(r/->AtomSpec) (r/->AnySpec)]
   [{:guard [{:id 0 :type (r/->PlaceholderSpec "A")}] :conclusion [[{:id 1 :type (r/->PlaceholderSpec "A")}]]}])
  => (contains {"[Y]" "Tuple(Atom)"}))


 )

(defn test-guard-true [term actual-type guard-type]
  (sut/is-guard-true?
   (-> (uber/digraph) (uber/add-nodes-with-attrs [term {:dom actual-type}]))
   {:arg term :type guard-type}))

(facts
 "About is-guard-true?"
 (fact
  "Simple Specs"
  (test-guard-true (r/->VarTerm "X") (r/->IntegerSpec) (r/->NumberSpec)) => {}
  )
 (fact
  "Any"
  (test-guard-true (r/->VarTerm "X") (r/->IntegerSpec) (r/->AnySpec)) => {})
 (fact
  "With Placeholder"
  (test-guard-true (r/->VarTerm "X") (r/->IntegerSpec) (r/->PlaceholderSpec "A")) => (contains {"A" (r/->IntegerSpec)})
  ))
