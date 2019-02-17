(ns prolog-analyzer.analyzer.built-in-specs
  (:require [prolog-analyzer.records :as r]))

(def built-in-specs
  {"member"
   {2
    {:pre-specs [[(r/make-spec:any) (r/make-spec:list (r/make-spec:any))]
                 [(r/make-spec:var) (r/make-spec:list (r/make-spec:any))]
                 [(r/make-spec:any) (r/make-spec:var)]
                 [(r/make-spec:var) (r/make-spec:var)]]
     :post-specs [[[(r/make-spec:nonvar) (r/make-spec:list (r/make-spec:nonvar))] [(r/make-spec:nonvar) (r/make-spec:list (r/make-spec:nonvar))]]
                  [[(r/make-spec:var) (r/make-spec:list (r/make-spec:specvar "X"))] [(r/make-spec:specvar "X") (r/make-spec:list (r/make-spec:specvar "X"))]]
                  [[(r/make-spec:specvar "X") (r/make-spec:var)] [(r/make-spec:specvar "X") (r/make-spec:list (r/make-spec:one-of [(r/make-spec:specvar "X") (r/make-spec:var)]))]]
                  [[(r/make-spec:var) (r/make-spec:var)] [(r/make-spec:var) (r/make-spec:list (r/make-spec:var))]]]
     :inv-specs []}}
   "is"
   {2
    {:pre-specs [[(r/make-spec:var) (r/make-spec:user-defined "expr")]
                 [(r/make-spec:number) (r/make-spec:user-defined "expr")]]
     :post-specs [[[(r/make-spec:any) (r/make-spec:user-defined "expr")] [(r/make-spec:number) (r/make-spec:user-defined "expr")]]]
     :inv-specs []}}
   "append"
   {3
    {:pre-specs [[(r/make-spec:var) (r/make-spec:var) (r/make-spec:var)]
                 [(r/make-spec:var) (r/make-spec:var) (r/make-spec:list (r/make-spec:any))]
                 [(r/make-spec:var) (r/make-spec:var) (r/make-spec:list (r/make-spec:any))]
                 [(r/make-spec:list (r/make-spec:any)) (r/make-spec:var) (r/make-spec:var)]
                 [(r/make-spec:var) (r/make-spec:list (r/make-spec:any)) (r/make-spec:list (r/make-spec:any))]
                 [(r/make-spec:list (r/make-spec:any)) (r/make-spec:var) (r/make-spec:list (r/make-spec:any))]
                 [(r/make-spec:list (r/make-spec:any)) (r/make-spec:list (r/make-spec:any)) (r/make-spec:var)]
                 [(r/make-spec:list (r/make-spec:any)) (r/make-spec:list (r/make-spec:any)) (r/make-spec:list (r/make-spec:any))]]
     :post-specs [[[(r/make-spec:any) (r/make-spec:any) (r/make-spec:list (r/make-spec:specvar "X"))] [(r/make-spec:list (r/make-spec:specvar "X")) (r/make-spec:list (r/make-spec:specvar "X")) (r/make-spec:list (r/make-spec:specvar "X"))]]
                  [[(r/make-spec:any) (r/make-spec:any) (r/make-spec:any)] [(r/make-spec:list (r/make-spec:specvar "X")) (r/make-spec:list (r/make-spec:specvar "X")) (r/make-spec:list (r/make-spec:specvar "X"))]]]
     :inv-specs []}}})


(defn default-specs [arity]
  {:pre-specs [(repeat arity (r/make-spec:any))]
   :post-specs [[(repeat arity (r/make-spec:any)) (repeat arity (r/make-spec:any))]]
   :inv-specs []})

(defn get-specs-of-built-in-pred [pred-name arity]
  (if (contains? built-in-specs pred-name)
    (get-in built-in-specs [pred-name arity])
    (default-specs arity)))
