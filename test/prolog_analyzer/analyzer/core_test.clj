(ns prolog-analyzer.analyzer.core-test
  (:require [prolog-analyzer.analyzer.core :as sut]
            [clojure.test :refer [deftest are]]))


(deftest valid-simple-types-test
  (are [spec arg] (sut/valid spec arg)
    {:spec :integer} {:value 2 :type :integer}
    {:spec :atom} {:term "a" :type :atom}
    {:spec :atom} {:term "+++" :type :atom}
    {:spec :atomic} {:term "a" :type :atom}
    {:spec :atomic} {:value 3.5 :type :float}
    {:spec :number} {:value 3.14 :type :float}
    {:spec :number} {:value 3 :type :integer}
    {:spec :ground} {:value 3 :type :integer}
    {:spec :ground} {:term "bla" :type :atom}
    {:spec :var} {:name "H" :type :var}
    {:spec :var} {:name "_3452" :type :anon_var}
    {:spec :nonvar} {:value 3 :type :integer}
    {:spec :nonvar} {:type :compound :functor "foo" :arglist [{:name "X" :type :var}]}
    )
  (are [spec arg] (not (sut/valid spec arg))
    {:spec :integer} {:term "a" :type :atom}
    {:spec :ground} {:name "H" :type :var}
    {:spec :var} {:value 3 :type :integer}
    {:spec :nonvar} {:name "CX" :type :var}
    {:spec :nonvar} {:name "_342" :type :anon_var}
    ))


(deftest valid-complex-types-test
  (are [spec arg] (sut/valid spec arg)
    {:spec :ground} {:type :compound :functor "bar" :arglist [{:term "a" :type :atom}]}
    {:spec :list :type {:spec :integer}} {:type :list :arglist [{:value 2 :type :integer} {:value 2 :type :integer}]}
    {:spec :list :type {:spec :integer}} {:type :list :arglist []}
    {:spec :tuple :arglist [{:spec :number} {:spec :atomic}]} {:type :list :arglist [{:value 2 :type :integer} {:term "a" :type :atom}]}
    )
  (are [spec arg] (not (sut/valid spec arg))
    {:spec :ground} {:type :compound :functor "bar" :arglist [{:term "a" :type :atom} {:name "X" :type :var}]}
    {:spec :list :type {:spec :integer}} {:type :list :arglist [{:name "X" :type :var}]}
    {:spec :list :type {:spec :integer}} {:type :head-tail-list :head {:value 2 :type :integer} :tail {:name "X" :type :var}}
    {:spec :tuple :arglist [{:spec :number}]} {:value 2 :type :integer}
    )
  )

;TODO is [1|T] a valid arg for spec `tuple([int])`?
