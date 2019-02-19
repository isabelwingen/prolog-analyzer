(ns prolog-analyzer.records
  (:require [clojure.tools.logging :as log]
            [clojure.string]))

(declare to-string)

(defn to-arglist [list]
  (clojure.string/join ", " (map to-string list)))

(defprotocol analyzer-datastructure
  (to-string [x]))

(defn get-elements-of-list [{head :head tail :tail}]
  (if (= "[]" (:term tail))
    (list head)
    (conj (get-elements-of-list tail) head)))


(defrecord Term [type]
  analyzer-datastructure
  (to-string [x]
    (case type
      (:anon_var, :var) (str (:name x))
      (:atom, :atomic) (str (:term x))
      (:integer, :number, :float) (str (:value x))
      :list (let [head (:head x)
                  tail (:tail x)]
              (cond
                (= "[]" (:term tail)) (str "[" (to-string head) "]")
                (= :var (:type tail)) (str "[" (to-string head) "|" (to-string tail) "]")
                (= :anon_var (:type tail)) (str "[" (to-string head) "|" (to-string tail) "]")
                (= :list (:type tail)) (str "[" (to-arglist (get-elements-of-list x)) "]")))
      :compound (str (:functor x) "(" (to-arglist (:arglist x)) ")")
      "PRINT ERROR TERM")))

(defn map-to-term [m]
  (case (:type m)
    (:anon_var, :var, :any, :ground, :nonvar, :atom, :atomic, :number, :integer, :float) (map->Term m)
    :list (map->Term (-> m
                         (update :head map-to-term)
                         (update :tail map-to-term)))
    :compound (map->Term (update m :arglist #(map map-to-term %)))
    (do
      (map->Term m)
      (log/error "No case for" m "in map-to-term"))))


(defn make-term:var [name]
  (assoc (Term. :var) :name name))

(defn make-term:anon_var [name]
  (assoc (Term. :anon_var) :name name))

(defn make-term:any [name]
  (assoc (Term. :any) :name name))

(defn make-term:ground [name]
  (assoc (Term. :ground) :name name))

(defn make-term:nonvar [name]
  (assoc (Term. :nonvar) :name name))

(defn make-term:atom [term]
  (assoc (Term. :atom) :term term))

(defn make-term:atomic [term]
  (assoc (Term. :atomic) :term term))

(defn make-term:number [value]
  (assoc (Term. :number) :value value))

(defn make-term:integer [value]
  (assoc (Term. :integer) :value value))

(defn make-term:float [value]
  (assoc (Term. :float) :value value))

(defn make-term:list [head tail]
  (-> (Term. :list)
      (assoc :head head)
      (assoc :tail tail)))

(defn make-term:compound [functor arglist]
  (-> (Term. :compound)
      (assoc :functor functor)
      (assoc :arglist arglist)))

(defrecord Spec [spec]
  analyzer-datastructure
  (to-string [x]
    (case spec
      :any "Any"
      :var "Var"
      :atom "Atom"
      :atomic "Atomic"
      :ground "Ground"
      :nonvar "Nonvar"
      :number "Number"
      :integer "Integer"
      :float "Float"
      :list (str "List(" (to-string (:type x)) ")")
      :tuple (str "Tuple(" (to-arglist (:arglist x)) ")")
      :exact (str "Exact(" (:value x) ")")
      :specvar (str "Specvar(" (:name x) ")")
      :compound (str (:functor x) "(" (to-arglist (:arglist x)) ")")
      :and (str "And(" (to-arglist (:arglist x)) ")")
      :one-of (str "OneOf(" (to-arglist (:arglist x)) ")")
      :user-defined (if (contains? x :arglist)
                      (str (:name x) "(" (to-arglist (:arglist x)) ")")
                      (str (:name x)))
      :error (str "ERROR: " (:reason x))
      "PRINT ERROR SPEC")))


(defn make-spec:var []
  (Spec. :var))

(defn make-spec:atom []
  (Spec. :atom))

(defn make-spec:atomic []
  (Spec. :atomic))

(defn make-spec:integer []
  (Spec. :integer))

(defn make-spec:float []
  (Spec. :float))

(defn make-spec:number []
  (Spec. :number))

(defn make-spec:ground []
  (Spec. :ground))

(defn make-spec:nonvar []
  (Spec. :nonvar))

(defn make-spec:any []
  (Spec. :any))

(defn make-spec:list [type]
  (assoc (Spec. :list) :type type))

(defn make-spec:tuple [arglist]
  (assoc (Spec. :tuple) :arglist arglist))

(defn make-spec:exact [value]
  (assoc (Spec. :exact) :value value))

(defn make-spec:specvar [name]
  (assoc (Spec. :specvar) :name name))

(defn make-spec:compound [functor arglist]
  (-> (Spec. :compound)
      (assoc :functor functor)
      (assoc :arglist arglist)))

(defn make-spec:one-of [arglist]
  (assoc (Spec. :one-of) :arglist arglist))

(defn make-spec:and [arglist]
  (assoc (Spec. :and) :arglist arglist))

(defn make-spec:user-defined
  ([name] (assoc (Spec. :user-defined) :name name))
  ([name arglist] (-> (Spec. :user-defined)
                      (assoc :name name)
                      (assoc :arglist arglist))))

(defn make-spec:error [reason]
  (assoc (Spec. :error) :reason reason))

(defn map-to-spec [m]
  (case (:spec m)
    (:var, :any, :ground, :nonvar, :atom, :atomic, :number, :integer, :float) (map->Spec m)
    :list (map->Spec (update m :type map-to-spec))
    :compound (map->Spec (update m :arglist #(map map-to-spec %)))
    (do
      (map->Spec m)
      (log/error "No case for" m "in map-to-term"))))
