(ns prolog-analyzer.analyzer.built-in-specs)

(def built-in-specs
  {"member"
   {2
    {:pre-specs [[{:spec :any} {:spec :list :type {:spec :any}}]
                 [{:spec :var} {:spec :list :type {:spec :any}}]
                 [{:spec :any} {:spec :var}]
                 [{:spec :var} {:spec :var}]]
     :post-specs [[[{:spec :nonvar} {:spec :list :type {:spec :nonvar}}] [{:spec :nonvar} {:spec :list :type {:spec :nonvar}}]]
                  [[{:spec :var} {:spec :list :type {:spec :named-any :name "X"}}] [{:spec :named-any :name "X"} {:spec :list :type {:spec :named-any :name "X"}}]]
                  [[{:spec :named-any :name "X"} {:spec :var}] [{:spec :named-any :name "X"} {:spec :list :type {:spec :or :arglist [{:spec :named-any :name "X"} {:spec :var}]}}]]
                  [[{:spec :var} {:spec :var}] [{:spec :var} {:spec :list :type {:spec :var}}]]]
     :inv-specs []}}
   "is"
   {2
    {:pre-specs [[{:spec :var} {:spec :expr}]
                 [{:spec :number} {:spec :expr}]]
     :post-specs [[[{:spec :any} {:spec :expr}] [{:spec :number} {:spec :expr}]]]
     :inv-specs []}}
   "append"
   {3
    {:pre-specs [[{:spec :var} {:spec :var} {:spec :var}]
                 [{:spec :var} {:spec :var} {:spec :list :type {:spec :any}}]
                 [{:spec :var} {:spec :var} {:spec :list :type {:spec :any}}]
                 [{:spec :list :type {:spec :any}} {:spec :var} {:spec :var}]
                 [{:spec :var} {:spec :list :type {:spec :any}} {:spec :list :type {:spec :any}}]
                 [{:spec :list :type {:spec :any}} {:spec :var} {:spec :list :type {:spec :any}}]
                 [{:spec :list :type {:spec :any}} {:spec :list :type {:spec :any}} {:spec :var}]
                 [{:spec :list :type {:spec :any}} {:spec :list :type {:spec :any}} {:spec :list :type {:spec :any}}]]
     :post-specs [[[{:spec :any} {:spec :any} {:spec :list :type {:spec :named-any :name "X"}}] [{:spec :list :type {:spec :named-any :name "X"}} {:spec :list :type {:spec :named-any :name "X"}} {:spec :list :type {:spec :named-any :name "X"}}]]
                  [[{:spec :any} {:spec :any} {:spec :any}] [{:spec :list :type {:spec :named-any :name "X"}} {:spec :list :type {:spec :named-any :name "X"}} {:spec :list :type {:spec :named-any :name "X"}}]]]
     :inv-specs []}}})

;:specs {{:spec "expr"} {:spec :one_of :arglist [{:spec :number} {:spec :compound :arglist [{:spec "expr"}]} {:spec :compound :arglist [{:spec "expr"} {:spec "expr"}]}]}}

(defn default-specs [arity]
  {:pre-specs [(repeat arity {:spec :any})]
   :post-specs [[(repeat arity {:spec :any}) (repeat arity {:spec :any})]]
   :inv-specs []})

(defn get-specs-of-built-in-pred [pred-name arity]
  (if (contains? built-in-specs pred-name)
    (get-in built-in-specs [pred-name arity])
    (default-specs arity)))
