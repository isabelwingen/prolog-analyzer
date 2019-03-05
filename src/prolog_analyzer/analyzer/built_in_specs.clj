(ns prolog-analyzer.analyzer.built-in-specs
  (:require [prolog-analyzer.records :as r]))

(def built-in-specs
  {"member"
   {2
    {:pre-specs [[(r/->AnySpec) (r/->ListSpec (r/->AnySpec))]
                 [(r/->VarSpec) (r/->ListSpec (r/->AnySpec))]
                 [(r/->AnySpec) (r/->VarSpec)]
                 [(r/->VarSpec) (r/->VarSpec)]]
     :post-specs [[[(r/->NonvarSpec) (r/->ListSpec (r/->NonvarSpec))] [(r/->NonvarSpec) (r/->ListSpec (r/->NonvarSpec))]]
                  [[(r/->VarSpec) (r/->ListSpec (r/->SpecvarSpec "X"))] [(r/->SpecvarSpec "X") (r/->ListSpec (r/->SpecvarSpec "X"))]]
                  [[(r/->SpecvarSpec "X") (r/->VarSpec)] [(r/->SpecvarSpec "X") (r/->ListSpec (r/->OneOfSpec [(r/->SpecvarSpec "X") (r/->VarSpec)]))]]
                  [[(r/->VarSpec) (r/->VarSpec)] [(r/->VarSpec) (r/->ListSpec (r/->VarSpec))]]]
     :inv-specs []}}
   "is"
   {2
    {:pre-specs [[(r/->VarSpec) (r/->UserDefinedSpec "expr")]
                 [(r/->NumberSpec) (r/->UserDefinedSpec "expr")]]
     :post-specs [[[(r/->AnySpec) (r/->UserDefinedSpec "expr")] [(r/->NumberSpec) (r/->UserDefinedSpec "expr")]]]
     :inv-specs []}}
   "append"
   {3
    {:pre-specs [[(r/->VarSpec) (r/->VarSpec) (r/->VarSpec)]
                 [(r/->VarSpec) (r/->VarSpec) (r/->ListSpec (r/->AnySpec))]
                 [(r/->VarSpec) (r/->VarSpec) (r/->ListSpec (r/->AnySpec))]
                 [(r/->ListSpec (r/->AnySpec)) (r/->VarSpec) (r/->VarSpec)]
                 [(r/->VarSpec) (r/->ListSpec (r/->AnySpec)) (r/->ListSpec (r/->AnySpec))]
                 [(r/->ListSpec (r/->AnySpec)) (r/->VarSpec) (r/->ListSpec (r/->AnySpec))]
                 [(r/->ListSpec (r/->AnySpec)) (r/->ListSpec (r/->AnySpec)) (r/->VarSpec)]
                 [(r/->ListSpec (r/->AnySpec)) (r/->ListSpec (r/->AnySpec)) (r/->ListSpec (r/->AnySpec))]]
     :post-specs [[[(r/->AnySpec) (r/->AnySpec) (r/->ListSpec (r/->SpecvarSpec "X"))] [(r/->ListSpec (r/->SpecvarSpec "X")) (r/->ListSpec (r/->SpecvarSpec "X")) (r/->ListSpec (r/->SpecvarSpec "X"))]]
                  [[(r/->AnySpec) (r/->AnySpec) (r/->AnySpec)] [(r/->ListSpec (r/->SpecvarSpec "X")) (r/->ListSpec (r/->SpecvarSpec "X")) (r/->ListSpec (r/->SpecvarSpec "X"))]]]
     :inv-specs []}}})


(defn default-specs [arity]
  {:pre-specs [(repeat arity (r/->AnySpec))]
   :post-specs [[(repeat arity (r/->AnySpec)) (repeat arity (r/->AnySpec))]]
   :inv-specs []})

(defn get-specs-of-built-in-pred [pred-name arity]
  (if (contains? built-in-specs pred-name)
    (get-in built-in-specs [pred-name arity])
    (default-specs arity)))

