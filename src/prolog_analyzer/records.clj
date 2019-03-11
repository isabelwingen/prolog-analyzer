(ns prolog-analyzer.records
  (:require [prolog-analyzer.utils :refer [case+ get-elements-of-list]]
            [clojure.tools.logging :as log]
            [clojure.tools.namespace.repl :refer [refresh]]
            [clojure.string]))

(def INTEGER :integer)
(def FLOAT :float)
(def NUMBER :number)
(def EXACT :exact)
(def ATOM :atom)
(def ATOMIC :atomic)
(def COMPOUND :compound)
(def LIST :list)
(def EMPTYLIST :empty-list)
(def TUPLE :tuple)
(def GROUND :ground)
(def NONVAR :nonvar)
(def VAR :var)

(def USERDEFINED :user-defined)
(def SPECVAR :specvar)
(def ERROR :error)

(def AND :and)
(def OR :one-of)

(def ANY :any)

(declare to-arglist)
(declare empty-list?)
(declare next-steps)
(declare spec-type)
(declare term-type)
(declare simplify-or)

(defn mark-spec [spec origin]
  (assoc spec :origin origin))

(defn copy-mark [from to]
  (assoc to :origin (:origin from)))

(defn get-mark [spec]
  (:origin spec))


(defprotocol printable
  (to-string [x]))

(defprotocol spec
  (spec-type [spec])
  (next-steps [spec term])
  (intersect [spec other-spec]))

(defprotocol term
  (term-type [term])
  (initial-spec [term]))

(defrecord AnySpec []
  spec
  (spec-type [spec] ANY)
  (next-steps [spec term]
    (if (contains? #{LIST, COMPOUND} (term-type term))
      [term (intersect spec (initial-spec term))]
      []))
  (intersect [spec other-spec] other-spec)
  printable
  (to-string [x] "Any"))


(defrecord ErrorSpec [reason]
  spec
  (spec-type [spec] ERROR)
  (next-steps [spec term] [])
  (intersect [spec other-spec] spec)
  printable
  (to-string [x] (str "ERROR: " reason)))

(def DISJOINT (->ErrorSpec "No Valid Intersection"))

(defn error-spec? [spec]
  (or (nil? spec)
      (= ERROR (spec-type spec))
      (if (contains? spec :type) (error-spec? (.type spec)))
      (if (contains? spec :arglist) (some error-spec? (:arglist spec)))))

(defn replace-error-spec-with-intersect-error [spec]
  (if (error-spec? spec)
    DISJOINT
    spec))

(defrecord VarSpec []
  spec
  (spec-type [spec] VAR)
  (next-steps [spec term] [])
  (intersect [spec other-spec]
    (case+ (spec-type other-spec)
           (VAR, ANY) spec
           (AND, OR) (intersect other-spec spec)
           USERDEFINED spec
           DISJOINT))
  printable
  (to-string [x] "Var"))

(defrecord EmptyListSpec []
  spec
  (spec-type [spec] EMPTYLIST)
  (next-steps [spec term] [])
  (intersect [spec other-spec]
    (case+ (spec-type other-spec)
           (EMPTYLIST, LIST, GROUND, NONVAR, ANY, ATOMIC) spec
           TUPLE (if (empty? (.arglist other-spec)) spec DISJOINT)
           (AND, OR) (intersect other-spec spec)
           USERDEFINED spec
           DISJOINT))
  printable
  (to-string [x] "EmptyList"))


(defrecord AtomSpec []
  spec
  (spec-type [spec] ATOM)
  (next-steps [spec term] [])
  (intersect [spec other-spec]
    (case+ (spec-type other-spec)
           (ATOM, ATOMIC, GROUND, NONVAR, ANY) spec
           EXACT other-spec
           (AND, OR) (intersect other-spec spec)
           USERDEFINED spec
           DISJOINT))
  printable
  (to-string [x] "Atom"))

(defrecord IntegerSpec []
  spec
  (spec-type [spec] INTEGER)
  (next-steps [spec term] [])
  (intersect [spec other-spec]
    (case+ (spec-type other-spec)
           (ATOMIC, INTEGER, NUMBER, GROUND, NONVAR, ANY) spec
           (AND, OR) (intersect other-spec spec)
           USERDEFINED spec
           DISJOINT
           ))
  printable
  (to-string [x] "Integer"))

(defrecord FloatSpec []
  spec
  (spec-type [spec] FLOAT)
  (next-steps [spec term] [])
  (intersect [spec other-spec]
    (case+ (spec-type other-spec)
           (ATOMIC, FLOAT, NUMBER, GROUND, NONVAR, ANY) spec
           (AND, OR) (intersect other-spec spec)
           USERDEFINED spec
           DISJOINT
           ))
  printable
  (to-string [x] "Float"))

(defrecord NumberSpec []
  spec
  (spec-type [spec] NUMBER)
  (next-steps [spec term] [])
  (intersect [spec other-spec]
    (case+ (spec-type other-spec)
           (ATOMIC, NUMBER, GROUND, NONVAR, ANY) spec
           (INTEGER, FLOAT) other-spec
           (AND, OR) (intersect other-spec spec)
           USERDEFINED spec
           DISJOINT))
  printable
  (to-string [x] "Number"))

(defrecord AtomicSpec []
  spec
  (spec-type [spec] ATOMIC)
  (next-steps [spec term] [])
  (intersect [spec other-spec]
    (case+ (spec-type other-spec)
           (ATOMIC, GROUND, NONVAR, ANY) spec
           (INTEGER, FLOAT, ATOM, NUMBER, EMPTYLIST, EXACT) other-spec
           LIST (->EmptyListSpec)
           (AND, OR) (intersect other-spec spec)
           USERDEFINED spec
           DISJOINT))
  printable
  (to-string [x] "Atomic"))


(defrecord ExactSpec [value]
  spec
  (spec-type [spec] EXACT)
  (next-steps [spec term] [])
  (intersect [spec other-spec]
    (case+ (spec-type other-spec)
           (GROUND, NONVAR, ANY, ATOM, ATOMIC) spec
           EXACT (if (= value (.value other-spec)) spec DISJOINT)
           (AND, OR) (intersect other-spec spec)
           USERDEFINED spec
           DISJOINT))
  printable
  (to-string [x] (str "Exact(" value ")")))


(defrecord ListSpec [type]
  spec
  (spec-type [spec] LIST)
  (next-steps [spec term]
    (if (= LIST (term-type term))
      (if (empty-list? (:tail term))
        [(.head term) type]
        [(.head term) type
         (.tail term) spec])
      []))
  (intersect [spec other-spec]
    (case+ (spec-type other-spec)
           LIST (-> other-spec
                    (update :type (partial intersect type))
                    replace-error-spec-with-intersect-error)
           TUPLE (-> other-spec
                     (update :arglist (partial map (partial intersect type)))
                     replace-error-spec-with-intersect-error)
           EMPTYLIST other-spec
           ATOMIC (->EmptyListSpec)
           (ANY, NONVAR) spec
           GROUND (replace-error-spec-with-intersect-error (update spec :type (partial intersect other-spec)))
           (AND, OR) (intersect other-spec spec)
           USERDEFINED spec
           DISJOINT))
  printable
  (to-string [x] (str "List(" (to-string type) ")")))

(defrecord TupleSpec [arglist]
  spec
  (spec-type [spec] TUPLE)
  (next-steps [spec term]
    (if (= LIST (term-type term))
      (if (empty-list? (:tail term))
        [(.head term) (first (.arglist spec))]
        [(.head term) (first (.arglist spec))
         (.tail term) (update spec :arglist rest)])
      []))
  (intersect [spec other-spec]
    (case+ (spec-type other-spec)
           EMPTYLIST (if (empty? arglist) other-spec DISJOINT)
           LIST (-> spec
                    (update :arglist (partial map (partial intersect (.type other-spec))))
                    replace-error-spec-with-intersect-error)
           TUPLE (if (= (count arglist) (count (.arglist other-spec)))
                   (-> other-spec
                       (update :arglist (partial map intersect arglist))
                       replace-error-spec-with-intersect-error)
                   DISJOINT)
           ATOMIC (if (empty? arglist) (->EmptyListSpec) DISJOINT)
           (ANY, NONVAR) spec
           GROUND (replace-error-spec-with-intersect-error (update spec :arglist (partial map (partial intersect other-spec))))
           (AND, OR) (intersect other-spec spec)
           USERDEFINED spec
           DISJOINT))
  printable
  (to-string [x] (str "Tuple(" (to-arglist arglist) ")")))


(defrecord CompoundSpec [functor arglist]
  spec
  (spec-type [spec] COMPOUND)
  (next-steps [spec term]
    (if (= COMPOUND (term-type term))
      (interleave (.arglist term) arglist)
      []))
  (intersect [spec other-spec]
    (case+ (spec-type other-spec)
           COMPOUND (if (and (= functor (.functor other-spec))
                             (= (count arglist) (count (.arglist other-spec))))
                      (-> other-spec
                          (update :arglist (partial map intersect arglist))
                          replace-error-spec-with-intersect-error)
                      DISJOINT)
           (ANY, NONVAR) spec
           GROUND (replace-error-spec-with-intersect-error (update spec :arglist (partial map (partial intersect other-spec))))
           (AND, OR) (intersect other-spec spec)
           USERDEFINED spec
           DISJOINT))
  printable
  (to-string [x] (str functor "(" (to-arglist arglist) ")")))

(defrecord GroundSpec []
  spec
  (spec-type [spec] GROUND)
  (next-steps [spec term]
    (if (contains? #{LIST, COMPOUND} (term-type term))
      [term (intersect spec (initial-spec term))]
      []))
  (intersect [spec other-spec]
    (case+ (spec-type other-spec)
     (ANY, NONVAR, GROUND) spec
     USERDEFINED spec
     (intersect other-spec spec)))
  printable
  (to-string [x] "Ground"))

(defn- simplify-and [{arglist :arglist}]
  (let [p (reduce intersect arglist)]
    (if (error-spec? p)
      DISJOINT
      p)))

(defn- simplify-or [spec]
  (let [simplified-or (-> spec
                          (update :arglist distinct)
                          (update :arglist (partial apply vector)))]
    (case (count (:arglist simplified-or))
      0 DISJOINT
      1 (first (:arglist simplified-or))
      (if (some #{ANY} (map spec-type (:arglist simplified-or)))
        (copy-mark spec (->AnySpec))
        simplified-or))))

(defrecord AndSpec [arglist]
  spec
  (spec-type [spec] AND)
  (next-steps [spec term]
    (if-let [suitable-spec (intersect spec (initial-spec term))]
      (if (= AND (spec-type suitable-spec))
        (interleave (repeat (count (.arglist suitable-spec)) term) (.arglist suitable-spec))
        [term suitable-spec])
      []))
  (intersect [spec other-spec]
    (case+ (spec-type other-spec)
           AND (-> other-spec
                   (update :arglist (partial concat arglist))
                   simplify-and
                   replace-error-spec-with-intersect-error)
           OR (-> other-spec
                  (update :arglist (partial map #(simplify-and (->AndSpec (conj arglist %)))))
                  simplify-or
                  replace-error-spec-with-intersect-error)
           (-> spec
               (update :arglist #(conj % other-spec))
               simplify-and
               replace-error-spec-with-intersect-error)))
  printable
  (to-string [x] (str "And(" (to-arglist arglist) ")")))

(defrecord OneOfSpec [arglist]
  spec
  (spec-type [spec] OR)
  (next-steps [spec term]
    (if-let [suitable-spec (intersect spec (initial-spec term))]
      (if (= OR (spec-type suitable-spec))
        []
        [term suitable-spec])
      []))
  (intersect [spec other-spec]
    (case+ (spec-type other-spec)
           OR (->> (for [x arglist
                         y (.arglist other-spec)]
                     [x y])
                   (map ->AndSpec)
                   (map simplify-and)
                   (apply vector)
                   ->OneOfSpec
                   simplify-or
                   replace-error-spec-with-intersect-error)
           AND (-> spec
                   (update :arglist (partial map #(simplify-and (->AndSpec (conj (.arglist other-spec) %)))))
                   simplify-or
                   replace-error-spec-with-intersect-error)
           (-> spec
               (update :arglist (partial map (partial intersect other-spec)))
               (update :arglist (partial remove error-spec?))
               simplify-or
               replace-error-spec-with-intersect-error
               )))
  printable
  (to-string [x] (str "OneOf(" (to-arglist arglist) ")")))


(defrecord UserDefinedSpec [name]
  spec
  (spec-type [spec] USERDEFINED)
  (next-steps [spec term] [])
  (intersect [spec other-spec] other-spec)
  printable
  (to-string [x] (if (contains? x :arglist)
                   (str name "(" (to-arglist (:arglist x)) ")")
                   (str name))))

(declare ->NonvarSpec)

(defrecord NonvarSpec []
  spec
  (spec-type [spec] NONVAR)
  (next-steps [spec term]
    (if (contains? #{LIST, COMPOUND} (term-type term))
      [term (intersect spec (initial-spec term))]
      []))
  (intersect [spec other-spec]
    (case+ (spec-type other-spec)
           (NONVAR, ANY) spec
           GROUND other-spec
           USERDEFINED spec
           (intersect other-spec spec)))
  printable
  (to-string [x] "Nonvar"))

(defrecord SpecvarSpec [name]
  spec
  (spec-type [spec] SPECVAR)
  (next-steps [spec term] [])
  (intersect [spec other-spec] other-spec)
  printable
  (to-string [x] (str "Specvar(" (apply str (drop 3 (str name))) ")")))

(apply str (take 2 "hallo"))

(defrecord VarTerm [name]
  term
  (term-type [term] VAR)
  (initial-spec [term] (->AnySpec))
  printable
  (to-string [x] (str name)))

(defrecord AnonVarTerm [name]
  term
  (term-type [term] VAR)
  (initial-spec [term] (->AnySpec))
  printable
  (to-string [x] (str name)))

(defrecord AtomTerm [term]
  term
  (term-type [term] ATOM)
  (initial-spec [term] (->AtomSpec))
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
  (initial-spec [term] (if (empty-list? tail)
                         (->ListSpec (initial-spec head))
                         (if (contains? tail :head)
                           (->ListSpec (simplify-or (->OneOfSpec [(initial-spec head) (:type (initial-spec tail))])))
                           (->ListSpec (->AnySpec)))))
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
  (initial-spec [term] (->CompoundSpec functor (map initial-spec arglist)))
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
    (simplify-or (->OneOfSpec specs))))

(defn to-arglist [list]
  (clojure.string/join ", " (map to-string list)))

(defn replace-specvar-name-with-value [spec specvar-name replace-value]
  (case+ (spec-type spec)
    SPECVAR
    (if (= specvar-name (:name spec)) (assoc spec :name replace-value) spec)

    (USERDEFINED, OR, AND, COMPOUND, TUPLE)
    (update spec :arglist (fn [s] (seq (map #(replace-specvar-name-with-value % specvar-name replace-value) s))))

    LIST
    (update spec :type #(replace-specvar-name-with-value % specvar-name replace-value))

    spec
    ))

(defn replace-specvars-with-spec [spec specvar-name replace-spec]
  (case+ (spec-type spec)
    SPECVAR
    (if (= specvar-name (:name spec)) replace-spec spec)

    (USERDEFINED, OR, AND, COMPOUND, TUPLE)
    (update spec :arglist (fn [s] (seq (map #(replace-specvars-with-spec % specvar-name replace-spec) s))))

    LIST
    (update spec :type #(replace-specvars-with-spec % specvar-name replace-spec))

    spec
    ))

(defn find-specvars [spec]
  (case+ (spec-type spec)
    SPECVAR [spec]
    (OR, AND, COMPOUND, TUPLE) (distinct (reduce concat (map find-specvars (.arglist spec))))
    USERDEFINED (if (contains? spec :arglist) (distinct (reduce concat (map find-specvars (:arglist spec)))) [])
    LIST (find-specvars (.type spec))
    []))
