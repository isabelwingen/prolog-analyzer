(ns prolog-analyzer.records
  (:require [prolog-analyzer.utils :refer [case+ get-elements-of-list]]
            [clojure.tools.logging :as log]
            [clojure.tools.namespace.repl :refer [refresh]]
            [clojure.string]))

(def INTEGER :integer)
(def FLOAT :float)
(def NUMBER :number)
(def ATOM :atom)
(def ATOMIC :atomic)
(def GROUND :ground)
(def ANY :any)
(def NONVAR :nonvar)
(def VAR :var)
(def LIST :list)
(def COMPOUND :compound)
(def TUPLE :tuple)
(def AND :and)
(def OR :one-of)
(def USERDEFINED :user-defined)
(def SPECVAR :specvar)
(def EXACT :exact)
(def ERROR :error)
(def EMPTYLIST :empty-list)

(declare to-arglist)
(declare empty-list?)
(declare suitable-spec)
(declare next-steps)


(defprotocol printable
  (to-string [x]))

(defprotocol spec
  (spec-type [spec])
  (suitable-spec [spec term])
  (next-steps [spec term]))

(defprotocol term
  (term-type [term])
  (initial-spec [term]))

(defrecord ErrorSpec [reason]
  spec
  (spec-type [spec] ERROR)
  (suitable-spec [spec term] spec)
  (next-steps [spec term] [])
  printable
  (to-string [x] (str "ERROR: " reason)))


(defrecord VarSpec []
  spec
  (spec-type [spec] VAR)
  (suitable-spec [spec term]
    (case+ (term-type term)
           (VAR, ANY) spec
           nil))
  (next-steps [spec term] [])
  printable
  (to-string [x] "Var"))

(defrecord EmptyListSpec []
  spec
  (spec-type [spec] EMPTYLIST)
  (suitable-spec [spec term]
    (if (contains? #{EMPTYLIST, VAR} (term-type term)) spec nil))
  (next-steps [spec term] [])
  printable
  (to-string [x] "EmptyList"))


(defrecord AtomSpec []
  spec
  (spec-type [spec] ATOM)
  (suitable-spec [spec term]
    (case+ (term-type term)
           (ATOM, VAR) spec
           ATOMIC (if (and (not= "[]" (:term term)) ((complement number?) (read-string (:term term)))) spec nil)
           nil))
  (next-steps [spec term] [])
  printable
  (to-string [x] "Atom"))

(defrecord IntegerSpec []
  spec
  (spec-type [spec] INTEGER)
  (suitable-spec [spec term]
    (case+ (term-type term)
           (INTEGER, VAR) spec
           NUMBER (if (int? (:value term)) spec nil)
           ATOMIC (if (int? (read-string (:term term))) spec nil)
           nil))
  (next-steps [spec term] [])
  printable
  (to-string [x] "Integer"))

(defrecord FloatSpec []
  spec
  (spec-type [spec] FLOAT)
  (suitable-spec [spec term]
    (case+ (term-type term)
           (FLOAT, VAR) spec
           NUMBER (if (float? (:value term)) spec nil)
           ATOMIC (if (float? (read-string (:term term))) spec nil)
           nil))
  (next-steps [spec term] [])
  printable
  (to-string [x] "Float"))

(defrecord NumberSpec []
  spec
  (spec-type [spec] NUMBER)
  (suitable-spec [spec term]
    (case+ (term-type term)
           (NUMBER, VAR) spec
           INTEGER (->IntegerSpec)
           FLOAT (->FloatSpec)
           ATOMIC (if (number? (read-string (:term term))) spec nil)
           nil))
  (next-steps [spec term] [])
  printable
  (to-string [x] "Number"))

(defrecord AtomicSpec []
  spec
  (spec-type [spec] ATOMIC)
  (suitable-spec [spec term]
    (case+ (term-type term)
           (ATOMIC, VAR) spec
           ATOM (->AtomSpec)
           NUMBER (->NumberSpec)
           INTEGER (->IntegerSpec)
           FLOAT (->FloatSpec)
           EMPTYLIST (->EmptyListSpec)
           nil))
  (next-steps [spec term] [])
  printable
  (to-string [x] "Atomic"))


(defrecord ExactSpec [value]
  spec
  (spec-type [spec] EXACT)
  (suitable-spec [spec term]
    (case+ (term-type term)
           VAR spec
           (ATOMIC, ATOM) (if (= value (:term term)) spec nil)
           nil))
  (next-steps [spec term] [])
  printable
  (to-string [x] (str "Exact(" value ")")))


(defrecord ListSpec [type]
  spec
  (spec-type [spec] LIST)
  (suitable-spec [spec term]
    (case+ (term-type term)
           VAR spec
           LIST (if (suitable-spec type (:head term)) spec nil)
           ATOMIC (if (= "[]" (:term term)) spec nil)
           EMPTYLIST (->EmptyListSpec)
           nil))
  (next-steps [spec term]
    (if (= LIST (term-type term))
      (if (empty-list? (:tail term))
        [(.head term) type]
        [(.head term) type
         (.tail term) spec])
      []))
  printable
  (to-string [x] (str "List(" (to-string type) ")")))

(defrecord TupleSpec [arglist]
  spec
  (spec-type [spec] TUPLE)
  (suitable-spec [spec term]
    (case+ (term-type term)
           VAR spec
           ATOMIC (if (and (empty-list? term) (empty? arglist)) spec nil)
           LIST (if (and (suitable-spec (first arglist) (:head term))
                         (suitable-spec (->TupleSpec (rest arglist)) (:tail term)))
                  spec
                  nil)
           EMPTYLIST (if (empty? arglist) (->EmptyListSpec) nil)
           nil))
  (next-steps [spec term]
    (if (= LIST (term-type term))
      (if (empty-list? (:tail term))
        [(.head term) (first (.arglist spec))]
        [(.head term) (first (.arglist spec))
         (.tail term) (update spec :arglist rest)])
      []))
  printable
  (to-string [x] (str "Tuple(" (to-arglist arglist) ")")))

(defrecord CompoundSpec [functor arglist]
  spec
  (spec-type [spec] COMPOUND)
  (suitable-spec [spec term]
    (case+ (term-type term)
           VAR spec
           COMPOUND (if (and (= functor (:functor term)) (= (count arglist) (count (:arglist term)))) spec nil)
           nil))
  (next-steps [spec term]
    (if (= COMPOUND (term-type term))
      (interleave (.arglist term) arglist)
      []))
  printable
  (to-string [x] (str functor "(" (to-arglist arglist) ")")))

(defrecord GroundSpec []
  spec
  (spec-type [spec] GROUND)
  (suitable-spec [spec term]
    (case+ (term-type term)
           (GROUND, NONVAR, ANY, VAR) spec
           ATOMIC (->AtomicSpec)
           ATOM (->AtomSpec)
           NUMBER (->NumberSpec)
           INTEGER (->IntegerSpec)
           FLOAT (->FloatSpec)
           LIST (->ListSpec (->GroundSpec))
           COMPOUND (->CompoundSpec (:functor term) (repeat (count (:arglist term)) (->GroundSpec)))
           EMPTYLIST (->EmptyListSpec)
           nil))
  (next-steps [spec term]
    (if (contains? #{LIST, COMPOUND} (term-type term))
      [term (suitable-spec spec term)]
      []))
  printable
  (to-string [x] "Ground"))

(defrecord AndSpec [arglist]
  spec
  (spec-type [spec] AND)
  (suitable-spec [spec term]
    (if (every? (complement nil?) (map #(suitable-spec % term) arglist))
      (let [mod-and (-> spec
                        (update :arglist (partial map #(suitable-spec % term)))
                        (update :arglist (partial map #(if (= AND (spec-type %)) (.arglist %) %)))
                        (update :arglist flatten)
                        (update :arglist distinct)
                        (update :arglist (partial apply vector)))]
        (case (count (:arglist mod-and))
          0 (->ErrorSpec "No valid components")
          1 (first (:arglist mod-and))
          mod-and))
      nil))
  (next-steps [spec term]
    (let [suitable-spec (suitable-spec spec term)]
      (if (= AND (spec-type suitable-spec))
        (interleave (repeat (count (.arglist suitable-spec)) term) (.arglist suitable-spec))
        [term suitable-spec])))
  printable
  (to-string [x] (str "And(" (to-arglist arglist) ")")))

(defrecord OneOfSpec [arglist]
  spec
  (spec-type [spec] OR)
  (suitable-spec [spec term]
    (let [simplified-or (-> spec
                            (update :arglist (partial map #(suitable-spec % term)))
                            (update :arglist (partial remove #(nil? %)))
                            (update :arglist distinct)
                            (update :arglist (partial apply vector)))]
      (case (count (:arglist simplified-or))
        0 (->ErrorSpec "No valid components")
        1 (first (.arglist simplified-or))
        simplified-or)))
  (next-steps [spec term]
    (let [suitable-spec (suitable-spec spec term)]
      (if (= OR (spec-type suitable-spec))
        []
        [term suitable-spec])))
  printable
  (to-string [x] (str "OneOf(" (to-arglist arglist) ")")))

(defrecord UserDefinedSpec [name]
  spec
  (spec-type [spec] USERDEFINED)
  (suitable-spec [spec term] spec)
  printable
  (to-string [x] (if (contains? x :arglist)
                   (str name "(" (to-arglist (:arglist x)) ")")
                   (str name))))

(declare ->NonvarSpec)

(defrecord AnySpec []
  spec
  (spec-type [spec] ANY)
  (suitable-spec [spec term]
    (case+ (term-type term)
           INTEGER (->IntegerSpec)
           FLOAT (->FloatSpec)
           NUMBER (->NumberSpec)
           ATOM (->AtomSpec)
           ATOMIC (->AtomicSpec)
           ANY (->AnySpec)
           GROUND (->GroundSpec)
           NONVAR (->NonvarSpec)
           VAR (->VarSpec)
           LIST (->ListSpec (->AnySpec))
           COMPOUND (->CompoundSpec (:functor term) (repeat (count (:arglist term)) (->AnySpec)))
           EMPTYLIST (->EmptyListSpec)
           nil))
  (next-steps [spec term]
    (if (contains? #{LIST, COMPOUND} (term-type term))
      [term (suitable-spec spec term)]
      []))
  printable
  (to-string [x] "Any"))

(defrecord NonvarSpec []
  spec
  (spec-type [spec] NONVAR)
  (suitable-spec [spec term]
    (case+ (term-type term)
           INTEGER (->IntegerSpec)
           FLOAT (->FloatSpec)
           NUMBER (->NumberSpec)
           ATOM (->AtomSpec)
           ATOMIC (->AtomicSpec)
           ANY (->NonvarSpec)
           GROUND (->GroundSpec)
           NONVAR (->NonvarSpec)
           VAR (->NonvarSpec)
           LIST (->ListSpec (->AnySpec))
           COMPOUND (->CompoundSpec (:functor term) (repeat (count (:arglist term)) (->AnySpec)))
           EMPTYLIST (->EmptyListSpec)
           nil))
  (next-steps [spec term]
    (if (contains? #{LIST, COMPOUND} (term-type term))
      [term (suitable-spec spec term)]
      []))
  printable
  (to-string [x] "Nonvar"))

(defrecord SpecvarSpec [name]
  spec
  (spec-type [spec] SPECVAR)
  (suitable-spec [spec term]
    (let [p (case+ (term-type term)
                   (GROUND, NONVAR, ATOM, ATOMIC, INTEGER, FLOAT, NUMBER) (initial-spec term)
                   VAR (->VarSpec)
                   ANY (->AnySpec)
                   LIST (->ListSpec (->AnySpec))
                   COMPOUND (->CompoundSpec (:functor term) (repeat (count (:arglist term)) (->AnySpec)))
                   EMPTYLIST (->EmptyListSpec)
                   nil
                   )]
      (->AndSpec [spec p])))
  (next-steps [spec term] [])
  printable
  (to-string [x] (str "Specvar(" name ")")))

(defrecord VarTerm [name]
  term
  (term-type [term] VAR)
  (initial-spec [term] (->VarSpec))
  printable
  (to-string [x] (str name)))

(defrecord AnonVarTerm [name]
  term
  (term-type [term] VAR)
  (initial-spec [term] (->VarSpec))
  printable
  (to-string [x] (str name)))

(defrecord AtomTerm [term]
  term
  (term-type [term] ATOM)
  (initial-spec [term] (->AtomSpec))
  printable
  (to-string [x] (str term)))

(defrecord AtomicTerm [term]
  term
  (term-type [term] ATOMIC)
  (initial-spec [term] (->AtomicSpec))
  printable
  (to-string [x] (str term)))

(defrecord EmptyListTerm []
  term
  (term-type [term] EMPTYLIST)
  (initial-spec [term] (->EmptyListSpec))
  printable
  (to-string [x] "[]"))


(defrecord IntegerTerm [value]
  term
  (term-type [term] INTEGER)
  (initial-spec [term] (->IntegerSpec))
  printable
  (to-string [x] (str value)))

(defrecord FloatTerm [value]
  term
  (term-type [term] FLOAT)
  (initial-spec [term] (->FloatSpec))
  printable
  (to-string [x] (str value)))

(defrecord NumberTerm [value]
  term
  (term-type [term] NUMBER)
  (initial-spec [term] (->NumberSpec))
  printable
  (to-string [x] (str value)))

(defrecord ListTerm [head tail]
  term
  (term-type [term] LIST)
  (initial-spec [term] (->ListSpec (->AnySpec)))
  printable
  (to-string [x]
    (case+ (term-type tail)
           ATOMIC (str "[" (to-string head) "]")
           EMPTYLIST (str "[" (to-string head) "]")
           VAR (str "[" (to-string head) "|" (to-string tail) "]")
           LIST (str "[" (to-arglist (get-elements-of-list x)) "]"))))

(defrecord CompoundTerm [functor arglist]
  term
  (term-type [term] COMPOUND)
  (initial-spec [term] (->CompoundSpec functor (repeat (count arglist) (->AnySpec))))
  printable
  (to-string [x] (str functor "(" (to-arglist arglist) ")")))

(defrecord ShouldNotHappenTerm [term]
  term
  (term-type [term] ERROR)
  (initial-spec [term] (->ErrorSpec (str "This term should not exists: " term)))
  printable
  (to-string [x] (str "ERROR: " term)))

(defn map-to-term [input-m]
  (let [m (dissoc input-m :type)]
    (case (:type input-m)
      :anon_var (map->AnonVarTerm m)
      :var (map->VarTerm m)
      :atom (map->AtomTerm m)
      :atomic (map->AtomicTerm m)
      :number (map->NumberTerm m)
      :integer (map->IntegerTerm m)
      :float (map->FloatTerm m)
      :list (map->ListTerm (-> m
                               (update :head map-to-term)
                               (update :tail map-to-term)))
      :compound (map->CompoundTerm (update m :arglist #(map map-to-term %)))
      :empty-list (->EmptyListTerm)
      :should-not-happen (map->ShouldNotHappenTerm m)
      (log/error "No case for" m "in map-to-term"))))

(defn map-to-spec [m]
  (case (:spec m)
    :var (map->VarSpec m)
    :any (map->AnySpec m)
    :ground (map->GroundSpec m)
    :nonvar (map->NonvarSpec m)
    :atom (map->AtomSpec m)
    :exact (map->ExactSpec m)
    :atomic (map->AtomicSpec m)
    :number (map->NumberSpec m)
    :integer (map->IntegerSpec m)
    :float (map->FloatSpec m)
    :list (map->ListSpec (update m :type map-to-spec))
    :tuple (map->TupleSpec (update m :arglist (partial map map-to-spec)))
    :compound (map->CompoundSpec (update m :arglist (partial map map-to-spec)))
    :and (map->AndSpec (update m :arglist (partial map map-to-spec)))
    :one-of (map->OneOfSpec (update m :arglist (partial map map-to-spec)))
    :user-defined (map->UserDefinedSpec (update m :arglist (partial map map-to-spec)))
    :error-spec (map->ErrorSpec m)
    :emptylist (->EmptyListSpec)
    (log/error "No case for" m "in map-to-term")))


(defn make-spec:var []
  (VarSpec.))

(defn make-spec:atom []
  (AtomSpec.))

(defn make-spec:atomic []
  (AtomicSpec.))

(defn make-spec:integer []
  (IntegerSpec.))

(defn make-spec:float []
  (FloatSpec.))

(defn make-spec:number []
  (NumberSpec.))

(defn make-spec:ground []
  (GroundSpec.))

(defn make-spec:nonvar []
  (NonvarSpec.))

(defn make-spec:any []
  (AnySpec.))

(defn make-spec:list [type]
  (ListSpec. type))

(defn make-spec:tuple [arglist]
  (TupleSpec. arglist))

(defn make-spec:exact [value]
  (ExactSpec. value))

(defn make-spec:specvar [name]
  (SpecvarSpec. name))

(defn make-spec:compound [functor arglist]
  (CompoundSpec. functor arglist))

(defn make-spec:one-of [arglist]
  (OneOfSpec. arglist))

(defn make-spec:and [arglist]
  (AndSpec. arglist))

(defn make-spec:user-defined
  ([name] (UserDefinedSpec. name))
  ([name arglist] (-> (UserDefinedSpec. name)
                      (assoc :arglist arglist))))

(defn make-spec:error [reason]
  (ErrorSpec. reason))


(defn empty-list?
  "Checks if the input is the empty (prolog) list."
  [term]
  (or (and (= ATOMIC (term-type term)) (= "[]" (:term term)))
      (= EMPTYLIST (term-type term))))

(defn to-head-tail-list
  "Transforms a bunch of `terms` to a proper prolog list."
  [& terms]
  (if (empty? terms)
    (->EmptyListTerm)
    (->ListTerm (first terms) (apply to-head-tail-list (rest terms)))))

(defn to-tuple-spec
  "Transforms a bunch of `specs` to a tuple spec."
  [& specs]
  (if (empty? specs)
    (->TupleSpec [])
    (->TupleSpec specs)))

(defn to-or-spec
  "Transforms a bunch of `specs` to a one-of spec."
  [& specs]
  (case (count specs)
    0 (->ErrorSpec "Cannot build empty one-of")
    1 (first specs)
    (->OneOfSpec specs)))

(defn to-arglist [list]
  (clojure.string/join ", " (map to-string list)))

(defn replace-specvar-name-with-value [spec specvar-name replace-value]
  (case (spec-type spec)
    SPECVAR
    (if (= specvar-name (:name spec)) (assoc spec :name replace-value) spec)

    (USERDEFINED, OR, AND, COMPOUND, TUPLE)
    (update spec :arglist (fn [s] (seq (map #(replace-specvar-name-with-value % specvar-name replace-value) s))))

    LIST
    (update spec :type #(replace-specvar-name-with-value % specvar-name replace-value))

    spec
    ))

(defn replace-specvars-with-spec [spec specvar-name replace-spec]
  (case (spec-type spec)
    SPECVAR
    (if (= specvar-name (:name spec)) replace-spec spec)

    (USERDEFINED, OR, AND, COMPOUND, TUPLE)
    (update spec :arglist (fn [s] (seq (map #(replace-specvars-with-spec % specvar-name replace-spec) s))))

    LIST
    (update spec :type #(replace-specvars-with-spec % specvar-name replace-spec))

    spec
    ))

(defn find-specvars [spec]
  (case (spec-type spec)
    SPECVAR [spec]
    (USERDEFINED, OR, AND, COMPOUND, TUPLE) (distinct (reduce concat (map find-specvars (:arglist spec))))
    LIST (find-specvars (spec-type spec))
    []))
