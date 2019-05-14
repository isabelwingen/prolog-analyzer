(ns prolog-analyzer.records
  (:require [prolog-analyzer.utils :refer [case+ get-elements-of-list recursive-check-condition] :as utils]
            [clojure.tools.logging :as log]
            [clojure.tools.namespace.repl :refer [refresh]]
            [clojure.string]))


(def INTEGER :integer)
(def FLOAT :float)
(def NUMBER :number)
(def EXACT :exact)
(def ATOM :atom)
(def STRING :string)
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
(def UNION :union)
(def COMPATIBLE :compatible)
(def ERROR :error)

(def AND :and)
(def OR :one-of)

(def ANY :any)
(def PLACEHOLDER :placeholder)

(declare to-arglist)
(declare empty-list?)
(declare next-steps)
(declare spec-type)
(declare term-type)
(declare simplify-or)
(declare resolve-definition-with-parameters)
(declare supertype?)
(declare ->AndSpec)
(declare has-specvars)
(declare var-spec?)

(defprotocol printable
  (to-string [x]))

(defprotocol spec
  (spec-type [spec])
  (next-steps
    [spec term defs]
    [spec term defs overwrite?])
  (intersect
    [spec other-spec defs]
    [spec other-spec defs overwrite?]))

(defprotocol term
  (term-type [term])
  (initial-spec [term]))

(defrecord AnySpec []
  spec
  (spec-type [spec] ANY)
  (next-steps [spec term defs overwrite?]
    (if (contains? #{LIST, COMPOUND} (term-type term))
      [term (intersect spec (initial-spec term) defs)]
      []))
  (next-steps [spec term defs] (next-steps spec term defs false))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           AND (intersect other-spec spec defs overwrite?)
           other-spec))
  printable
  (to-string [x] "Any"))


(defrecord ErrorSpec [reason]
  spec
  (spec-type [spec] ERROR)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  (intersect [spec other-spec defs _]
    (if (= ERROR (spec-type other-spec))
      other-spec
      spec))
  printable
  (to-string [x] (str "ERROR: " reason)))

(defn DISJOINT
  ([] (->ErrorSpec (str "No valid intersection")))
  ([a] (->ErrorSpec (str "No valid intersection of " (to-string a))))
  ([a b]
   (->ErrorSpec (str "No valid intersection of " (to-string a) " and " (to-string b)))))


(defn error-spec? [spec]
  (or (nil? spec)
      (= ERROR (spec-type spec))
      (if (contains? spec :type) (error-spec? (.type spec)) false)
      (if (contains? spec :arglist) (some error-spec? (:arglist spec)) false)
      ))

(defn replace-error-spec-with-intersect-error [spec]
  (if (error-spec? spec)
    (DISJOINT)
    spec))



(defrecord VarSpec []
  spec
  (spec-type [spec] VAR)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs]
    (case+ (spec-type other-spec)
           (VAR, ANY) spec
           (AND, OR) (intersect other-spec spec defs)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs)
           PLACEHOLDER (intersect other-spec spec defs)
           ERROR other-spec
           SPECVAR spec
           (DISJOINT spec other-spec)))
  (intersect [spec other-spec defs overwrite?]
    (if overwrite?
      (intersect (->AnySpec) other-spec defs overwrite?)
      (intersect spec other-spec defs)))
  printable
  (to-string [x] "Var"))

(defrecord EmptyListSpec []
  spec
  (spec-type [spec] EMPTYLIST)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           (EMPTYLIST, LIST, GROUND, NONVAR, ANY, ATOMIC) spec
           TUPLE (if (empty? (.arglist other-spec)) spec (DISJOINT spec other-spec))
           COMPOUND (if (nil? (:functor other-spec))
                      spec
                      (DISJOINT other-spec spec))
           (AND, OR) (intersect other-spec spec defs overwrite?)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           PLACEHOLDER (intersect other-spec spec defs overwrite?)
           ERROR other-spec
           VAR (if overwrite? spec (DISJOINT spec other-spec))
           SPECVAR spec
           (DISJOINT spec other-spec)))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] "EmptyList"))

(defrecord StringSpec []
  spec
  (spec-type [spec] STRING)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           (STRING, ATOMIC, GROUND, NONVAR, ANY) spec
           (AND, OR) (intersect other-spec spec defs overwrite?)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           PLACEHOLDER (intersect other-spec spec defs overwrite?)
           ERROR other-spec
           VAR (if overwrite? spec (DISJOINT spec other-spec))
           SPECVAR spec
           (DISJOINT spec other-spec)))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] "String"))

(defrecord AtomSpec []
  spec
  (spec-type [spec] ATOM)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           (ATOM, ATOMIC, GROUND, NONVAR, ANY) spec
           EXACT other-spec
           (AND, OR) (intersect other-spec spec defs overwrite?)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           SPECVAR spec
           PLACEHOLDER (intersect other-spec spec defs overwrite?)
           ERROR other-spec
           VAR (if overwrite? spec (DISJOINT spec other-spec))
           (DISJOINT spec other-spec)))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] "Atom"))

(defrecord IntegerSpec []
  spec
  (spec-type [spec] INTEGER)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           (ATOMIC, INTEGER, NUMBER, GROUND, NONVAR, ANY) spec
           (AND, OR) (intersect other-spec spec defs overwrite?)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           SPECVAR spec
           PLACEHOLDER (intersect other-spec spec defs overwrite?)
           ERROR other-spec
           VAR (if overwrite? spec (DISJOINT spec other-spec))
           (DISJOINT spec other-spec)
           ))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] "Integer"))

(defrecord FloatSpec []
  spec
  (spec-type [spec] FLOAT)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           (ATOMIC, FLOAT, NUMBER, GROUND, NONVAR, ANY) spec
           (AND, OR) (intersect other-spec spec defs overwrite?)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           SPECVAR spec
           PLACEHOLDER (intersect other-spec spec defs overwrite?)
           ERROR other-spec
           VAR (if overwrite? spec (DISJOINT spec other-spec))
           (DISJOINT spec other-spec)
           ))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] "Float"))

(defrecord NumberSpec []
  spec
  (spec-type [spec] NUMBER)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           (ATOMIC, NUMBER, GROUND, NONVAR, ANY) spec
           (INTEGER, FLOAT) other-spec
           (AND, OR) (intersect other-spec spec defs overwrite?)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           PLACEHOLDER (intersect other-spec spec defs overwrite?)
           SPECVAR spec
           ERROR other-spec
           VAR (if overwrite? spec (DISJOINT spec other-spec))
           (DISJOINT spec other-spec)))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] "Number"))

(defrecord AtomicSpec []
  spec
  (spec-type [spec] ATOMIC)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           (ATOMIC, GROUND, NONVAR, ANY) spec
           (INTEGER, FLOAT, ATOM, NUMBER, EMPTYLIST, EXACT, STRING) other-spec
           LIST (->EmptyListSpec)
           (AND, OR) (intersect other-spec spec defs overwrite?)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           SPECVAR spec
           VAR (if overwrite? spec (DISJOINT spec other-spec))
           ERROR other-spec
           PLACEHOLDER (intersect other-spec spec defs overwrite?)
           (DISJOINT spec other-spec)))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] "Atomic"))


(defrecord ExactSpec [value]
  spec
  (spec-type [spec] EXACT)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           (GROUND, NONVAR, ANY, ATOM, ATOMIC) spec
           EXACT (if (= value (.value other-spec)) spec (DISJOINT spec other-spec))
           (AND, OR) (intersect other-spec spec defs overwrite?)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           SPECVAR spec
           VAR (if overwrite? spec (DISJOINT spec other-spec))
           ERROR other-spec
           PLACEHOLDER (intersect other-spec spec defs overwrite?)
           (DISJOINT spec other-spec)))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] (str "Exact(" value ")")))

(defn list-term? [term]
  (and (= COMPOUND (term-type term))
       (= "." (.functor term))
       (= 2 (count (.arglist term)))))

(defn get-head-and-tail [term]
  {:head (first (.arglist term)) :tail (second (.arglist term))})

(def VAR-TAIL (->ErrorSpec "Tail of list is a variable"))


(defrecord ListSpec [type]
  spec
  (spec-type [spec] LIST)
  (next-steps [spec term defs overwrite]
    (cond
      (list-term? term) (let [{head :head tail :tail} (get-head-and-tail term)]
                          [head type tail spec])
      (= LIST (term-type term)) (if (or (empty-list? (.tail term)) (= VAR (term-type (.tail term)))) [(.head term) type] [(.head term) type (.tail term) spec])
      :else []))
  (next-steps [spec term defs] (next-steps spec term defs false))
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           LIST (-> other-spec
                    (update :type #(intersect type % defs overwrite?))
                    replace-error-spec-with-intersect-error)
           TUPLE (-> other-spec
                     (update :arglist (partial map #(intersect type % defs overwrite?)))
                     replace-error-spec-with-intersect-error)
           EMPTYLIST other-spec
           COMPOUND (if (nil? (.functor other-spec))
                      spec
                      (if (and (= "." (.functor other-spec)) (= 2 (count (:arglist other-spec))))
                        (-> spec
                            (update :type #(intersect % (first (.arglist other-spec)) defs overwrite?))
                            (intersect (second (.arglist other-spec)) defs overwrite?))
                        (DISJOINT spec other-spec)))
           ATOMIC (->EmptyListSpec)
           (ANY, NONVAR) spec
           GROUND (replace-error-spec-with-intersect-error (update spec :type #(intersect other-spec % defs overwrite?)))
           (AND, OR) (intersect other-spec spec defs overwrite?)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           SPECVAR spec
           VAR (if overwrite? spec (DISJOINT spec other-spec))
           ERROR other-spec
           PLACEHOLDER (intersect other-spec spec defs overwrite?)
           (DISJOINT spec other-spec)))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] (str "List(" (to-string type) ")")))

(defrecord TupleSpec [arglist]
  spec
  (spec-type [spec] TUPLE)
  (next-steps [spec term defs overwrite]
    (cond
      (list-term? term) (let [{head :head tail :tail} (get-head-and-tail term)]
                          [head (first arglist)
                           tail (update spec :arglist rest)])
      (= LIST (term-type term)) (if (empty? arglist) [(.head term) (->ErrorSpec "Empty Tuple")] [(.head term) (first arglist) (.tail term) (update spec :arglist rest)])
      :else []))
  (next-steps [spec term defs] (next-steps spec term defs false))
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           EMPTYLIST (if (empty? arglist) other-spec (DISJOINT spec other-spec))
           LIST (-> spec
                    (update :arglist (partial map #(intersect (.type other-spec) % defs overwrite?)))
                    replace-error-spec-with-intersect-error)
           TUPLE (if (= (count arglist) (count (.arglist other-spec)))
                   (-> other-spec
                       (update :arglist (partial map #(intersect %1 %2 defs overwrite?) arglist))
                       replace-error-spec-with-intersect-error)
                   (DISJOINT spec other-spec))
           COMPOUND (if (nil? (.functor other-spec))
                      spec
                      (if (and (= "." (.functor other-spec)) (= 2 (count (.arglist other-spec))))
                        (let [f (intersect (first (.arglist spec)) (first (.arglist other-spec)) defs overwrite?)
                              r (intersect (update :spec :arglist rest) (second (.arglist other-spec)) defs overwrite?)]
                          (assoc spec :arglist (apply vector f r)))
                        (DISJOINT spec other-spec)))
           ATOMIC (if (empty? arglist) (->EmptyListSpec) (DISJOINT spec other-spec))
           (ANY, NONVAR) spec
           GROUND (replace-error-spec-with-intersect-error (update spec :arglist (partial map #(intersect other-spec % defs overwrite?))))
           (AND, OR) (intersect other-spec spec defs overwrite?)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           SPECVAR spec
           VAR (if overwrite? spec (DISJOINT spec other-spec))
           PLACEHOLDER (intersect other-spec spec defs overwrite?)
           ERROR other-spec
           (DISJOINT spec other-spec)))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] (str "Tuple(" (to-arglist arglist) ")")))


(defrecord CompoundSpec [functor arglist]
  spec
  (spec-type [spec] COMPOUND)
  (next-steps [spec term defs _]
    (cond
      (= COMPOUND (term-type term)) (if (= (count arglist) (count (:arglist term)))
                                      (interleave (.arglist term) arglist)
                                      [])
      (and (= "." functor) (= 2 (count arglist)) (= LIST (term-type term))) [(.head term) (first arglist) (.tail term) (second arglist)]
      (and (= "." functor) (= 2 (count arglist)) (list-term? term)) [(first (.arglist term)) (first arglist) (second (.arglist term)) (second arglist)]
      :else []
      )
    )
  (next-steps [spec term defs] (next-steps spec term defs false))
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           COMPOUND (cond
                      (nil? functor) other-spec
                      (nil? (.functor other-spec)) spec
                      :else (if (and (= functor (.functor other-spec))
                                     (= (count arglist) (count (.arglist other-spec))))
                              (-> other-spec
                                  (update :arglist (partial map #(intersect %1 %2 defs overwrite?) arglist))
                                  replace-error-spec-with-intersect-error)
                              (DISJOINT spec other-spec)))
           LIST (if (nil? functor)
                  other-spec
                  (if (and (= "." functor) (= 2 (count arglist)))
                    (-> other-spec
                        (update :type #(intersect % (first arglist) defs overwrite?))
                        (intersect (second arglist) defs overwrite?))
                    (DISJOINT spec other-spec)))
           TUPLE (if (nil? functor)
                   other-spec
                   (if (and (= "." functor) (= 2 (count arglist)))
                     (let [f (intersect (first (.arglist other-spec)) (first arglist) defs overwrite?)
                           r (intersect (update other-spec :arglist rest) (second arglist) defs overwrite?)]
                       (assoc other-spec :arglist (apply vector f r)))
                     (DISJOINT spec other-spec)))
           EMPTYLIST (if (nil? functor)
                       other-spec
                       (DISJOINT spec other-spec))
           (ANY, NONVAR) spec
           GROUND (replace-error-spec-with-intersect-error (update spec :arglist (partial map #(intersect other-spec % defs overwrite?))))
           (AND, OR) (intersect other-spec spec defs overwrite?)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           SPECVAR spec
           VAR (if overwrite? spec (DISJOINT spec other-spec))
           ERROR other-spec
           PLACEHOLDER (intersect other-spec spec defs overwrite?)
           (DISJOINT spec other-spec)))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] (if (nil? functor) "Compound" (str "Compound(" functor "(" (to-arglist arglist) "))"))))

(declare ->GroundSpec)


(defn contains-vars?
  [spec defs already-resolved]
  (let [new-resolved (conj already-resolved spec)
        working (if (:arglist spec) spec (resolve-definition-with-parameters spec defs))]
    (if (= USERDEFINED (spec-type spec))
      (if (contains? already-resolved spec)
        false
        (->> working
             :arglist
             (remove #(contains? already-resolved %))
             (remove #(= spec %))
             (some #(contains-vars? % defs new-resolved))))
      (or (= VAR (spec-type spec))
          (some #(contains-vars? % defs new-resolved) (:arglist spec))))))



(defn helper:userdef->ground [spec defs overwrite? alread-done]
  (let [resolved (if (= USERDEFINED (spec-type spec))
                   (resolve-definition-with-parameters spec defs)
                   spec)
        working (if (:arglist spec) spec resolved)
        return (if (contains-vars? spec defs #{})
                 (if (nil? (:arglist working))
                   (intersect (->GroundSpec) working defs overwrite?)
                   (update working :arglist (partial reduce (fn [done new] (conj done (helper:userdef->ground new defs overwrite? (set (conj done new))))) [])))
                 spec)]
    (if (= OR (spec-type return))
      (simplify-or return defs)
      return)))



(defn userdef->ground [spec defs overwrite?]
  (helper:userdef->ground spec defs overwrite? #{}))

(defrecord GroundSpec []
  spec
  (spec-type [spec] GROUND)
  (next-steps [spec term defs overwrite?]
    (if (contains? #{LIST, COMPOUND} (term-type term))
      [term (intersect spec (initial-spec term) defs overwrite?)]
      []))
  (next-steps [spec term defs] (next-steps spec term defs false))
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
     (ANY, NONVAR, GROUND) spec
     USERDEFINED (userdef->ground other-spec defs overwrite?)
     SPECVAR spec
     VAR (if overwrite? spec (DISJOINT spec other-spec))
     ERROR other-spec
     (intersect other-spec spec defs overwrite?)))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] "Ground"))


(defn simplify-and [{arglist :arglist :as sp} defs overwrite?]
  (let [intersect (some->> arglist
                           set
                           (reduce #(intersect %1 %2 defs overwrite?)))]
    (if (error-spec? intersect)
      (DISJOINT sp)
      intersect)))


(defn add-to-one-of [defs so-far e]
  (assert e "!!")
  (let [one-direction (set (remove (fn [x] (supertype? defs e x)) so-far))]
    (if (empty? one-direction)
      [e]
      (if (every? #(not (supertype? defs % e)) one-direction)
        (set (conj one-direction e))
        one-direction))))

(defn simplify-or [type defs]
  (let [simplified-or (-> type
                          (update :arglist (partial mapcat #(if (= OR (spec-type %)) (:arglist %) [%])))
                          (update :arglist set)
                          (update :arglist #(reduce (partial add-to-one-of defs) [(first %)] (rest %)))
                          (update :arglist set))]
    (case (count (:arglist simplified-or))
      0 (DISJOINT type)
      1 (first (:arglist simplified-or))
      (if (some #{ANY} (map spec-type (:arglist simplified-or)))
        (->AnySpec)
        simplified-or))))

(defn simple-simplify-or [type]
  (let [res (-> type
                (update :arglist (partial mapcat #(if (= OR (spec-type %)) (:arglist %) [%])))
                (update :arglist set))]
    (case (count (:arglist res))
      0 (DISJOINT type)
      1 (first (:arglist res))
      (if (some #{ANY} (map spec-type (:arglist res)))
        (->AnySpec)
        res))))


(defrecord AndSpec [arglist]
  spec
  (spec-type [spec] AND)
  (next-steps [spec term defs overwrite?]
    (interleave (repeat term) arglist))
  (next-steps [spec term defs] (next-steps spec term defs false))
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           AND (-> other-spec
                   (update :arglist (partial concat arglist))
                   (simplify-and defs overwrite?)
                   replace-error-spec-with-intersect-error)
           OR (-> other-spec
                  (update :arglist (partial map #(->AndSpec (conj arglist %))))
                  (update :arglist (partial map #(simplify-and % defs overwrite?)))
                  (update :arglist (partial remove error-spec?))
                  (simplify-or defs)
                  replace-error-spec-with-intersect-error)
           ERROR other-spec
           (-> spec
               (update :arglist #(conj % other-spec))
               (simplify-and defs overwrite?)
               replace-error-spec-with-intersect-error)))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] (str "And(" (to-arglist arglist) ")")))

(defrecord OneOfSpec [arglist]
  spec
  (spec-type [spec] OR)
  (next-steps [spec term defs overwrite?]
    (if-let [{arglist :arglist :as suitable-spec} (intersect spec (initial-spec term) defs overwrite?)]
      (if (= OR (spec-type suitable-spec))
        (do
          (recursive-check-condition arglist "Next Steps: One Of - an argument was nil")
          (->> arglist
               (map #(next-steps % term defs overwrite?))
               (map (partial apply hash-map))
               (map (partial reduce-kv (fn [m k v] (update m k #(set (conj % v)))) {}))
               (apply merge-with into)
               (reduce-kv (fn [m k v] (conj m k (simplify-or (->OneOfSpec (apply vector v)) defs))) [])))
        [term suitable-spec])
      []))
  (next-steps [spec term defs] (next-steps spec term defs false))
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           OR (let [type-list (->> (for [x arglist
                                         y (.arglist other-spec)]
                                     [x y])
                                   (map ->AndSpec)
                                   (map #(simplify-and % defs overwrite?))
                                   (remove error-spec?)
                                   (apply vector))]
                (if (empty? type-list)
                  (DISJOINT)
                  (->> type-list
                       ->OneOfSpec
                       (#(simplify-or % defs)))))
           AND (let [type (-> spec
                              (update :arglist (partial map #(->AndSpec (conj (.arglist other-spec) %))))
                              (update :arglist (partial map #(simplify-and % defs overwrite?)))
                              (update :arglist (partial remove error-spec?)))]
                 (if (empty? (:arglist type))
                   (DISJOINT)
                   (-> type
                       (simplify-or defs)
                       replace-error-spec-with-intersect-error)))
           ERROR other-spec
           (let [type (-> spec
                          (update :arglist (partial map #(intersect other-spec % defs overwrite?)))
                          (update :arglist (partial remove error-spec?)))]
             (if (empty? (:arglist type))
               (DISJOINT)
               (-> type
                   (simplify-or defs)
                   replace-error-spec-with-intersect-error)))))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] (str "OneOf(" (to-arglist arglist) ")")))



(defn- intersect-userdef-with-userdef [userdef1 userdef2 defs overwrite?]
  (if (and (= (:name userdef1) (:name userdef2))
           (= (count (:arglist userdef1)) (count (:arglist userdef2))))
    (if (nil? (:arglist userdef1))
      userdef1
      (update userdef1 :arglist (partial map #(intersect %1 %2 defs overwrite?) (:arglist userdef2))))
    (let [def1 (resolve-definition-with-parameters userdef1 defs)
          def2 (resolve-definition-with-parameters userdef2 defs)]
      (intersect def1 def2 defs overwrite?))))

(defrecord UserDefinedSpec [name]
  spec
  (spec-type [spec] USERDEFINED)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           ANY spec
           USERDEFINED (intersect-userdef-with-userdef spec other-spec defs overwrite?)
           SPECVAR spec
           GROUND (intersect other-spec spec defs overwrite?)
           (AND, OR, VAR) (intersect other-spec (resolve-definition-with-parameters spec defs) defs overwrite?)
           ERROR other-spec
           (intersect (resolve-definition-with-parameters spec defs) other-spec defs overwrite?)))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] (if (contains? x :arglist)
                   (str name "(" (to-arglist (:arglist x)) ")")
                   (str name))))

(defrecord NonvarSpec []
  spec
  (spec-type [spec] NONVAR)
  (next-steps [spec term defs overwrite?]
    (if (contains? #{LIST, COMPOUND} (term-type term))
      [term (intersect spec (initial-spec term) defs overwrite?)]
      []))
  (next-steps [spec term defs] (next-steps spec term defs false))
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           (NONVAR, ANY) spec
           GROUND other-spec
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           SPECVAR spec
           VAR (if overwrite? spec (DISJOINT spec other-spec))
           ERROR other-spec
           (intersect other-spec spec defs overwrite?)))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] "Nonvar"))

(defrecord SpecvarSpec [name]
  spec
  (spec-type [spec] SPECVAR)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs overwrite?] other-spec)
  (intersect [spec other-spec defs] other-spec)
  printable
  (to-string [x] (str "Specvar(" (if (.startsWith (str name) "G__") (apply str (drop 3 (str name))) (str name)) ")")))

(defrecord UnionSpec [name]
  spec
  (spec-type [spec] UNION)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs overwrite?] other-spec)
  (intersect [spec other-spec defs] other-spec)
  printable
  (to-string [x] (str "Union(" (if (.startsWith (str name) "G__") (apply str (drop 3 (str name))) (str name)) ")")))

(defrecord CompatibleSpec [name]
  spec
  (spec-type [spec] COMPATIBLE)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs overwrite?] other-spec)
  (intersect [spec other-spec defs] other-spec)
  printable
  (to-string [x] (str "Compatible(" (if (.startsWith (str name) "G__") (apply str (drop 3 (str name))) (str name)) ")")))

(defn simplify-and-without-intersect [type]
  (let [and (->> type
                 :arglist
                 (mapcat #(if (= AND (spec-type %)) (:arglist %) [%]))
                 set
                 ->AndSpec)]
    (if (= 1 (count (:arglist and)))
      (first (.arglist and))
      and)))


(defrecord PlaceholderSpec [inner-spec]
  spec
  (spec-type [spec] PLACEHOLDER)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           ANY spec
           (assoc spec :alias other-spec)))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] (str "Placeholder(" (to-string inner-spec) ")")))


(defrecord VarTerm [name]
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
  (to-string [x] (if (nil? term) "<atom>" (str term))))

(defrecord StringTerm [term]
  term
  (term-type [term] STRING)
  (initial-spec [term] (->StringSpec))
  printable
  (to-string [x] (str "\"" term "\"")))


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
                           (->ListSpec (->OneOfSpec [(initial-spec head) (:type (initial-spec tail))]))
                           (->ListSpec (->AnySpec)))))
  printable
  (to-string [x]
    (case+ (term-type tail)
           ATOMIC (str "[" (to-string head) "]")
           EMPTYLIST (str "[" (to-string head) "]")
           VAR (str "[" (to-string head) "|" (to-string tail) "]")
           LIST (str "[" (to-arglist (get-elements-of-list x)) "]")
           (str "[" (to-string head) "|" (to-string tail) "]"))))


(defrecord CompoundTerm [functor arglist]
  term
  (term-type [term] COMPOUND)
  (initial-spec [term] (->CompoundSpec functor (map initial-spec arglist)))
  printable
  (to-string [x] (str "Compound(" functor "(" (to-arglist arglist) "))")))

(defrecord ShouldNotHappenTerm [term]
  term
  (term-type [term] ERROR)
  (initial-spec [term] (->ErrorSpec (str "This term should not exists: " term)))
  printable
  (to-string [x] (str "ERROR: " term)))

(defn map-to-term [input-m]
  (if (not (map? input-m))
    (log/error (str input-m) " is " (type input-m))
    (let [m (dissoc input-m :type)]
      (case (:type input-m)
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
        :string (map->StringTerm m)
        :should-not-happen (map->ShouldNotHappenTerm m)
        (do
          (log/error "No case for" input-m "in map-to-term")
          (->AtomTerm "ERROR"))))))

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
    :string (->StringSpec)
    :list (map->ListSpec (update m :type map-to-spec))
    :tuple (map->TupleSpec (update m :arglist (partial map map-to-spec)))
    :compound (map->CompoundSpec (update m :arglist (partial map map-to-spec)))
    :and (map->AndSpec (-> m
                           (update :arglist (partial map map-to-spec))
                           (update :arglist set)))
    :one-of (map->OneOfSpec (-> m
                                (update :arglist (partial map map-to-spec))
                                (update :arglist set)))
    :user-defined (map->UserDefinedSpec (update m :arglist (partial map map-to-spec)))
    :error-spec (map->ErrorSpec m)
    :emptylist (->EmptyListSpec)
    (do
      (log/error "No case for" m "in map-to-spec")
      (->AnySpec))))


(defn make-spec:user-defined
  ([name] (->UserDefinedSpec name))
  ([name arglist] (-> (->UserDefinedSpec name)
                      (assoc :arglist arglist))))


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
  [defs & specs]
  (case (count specs)
    0 (->ErrorSpec "Cannot build empty one-of")
    1 (first specs)
    (simplify-or (->OneOfSpec (set specs)) defs)))

(defn to-arglist [list]
  (clojure.string/join ", " (map to-string list)))

(defn replace-specvar-name-with-value [spec specvar-name replace-value]
  (case+ (spec-type spec)
         (SPECVAR,UNION,COMPATIBLE)
         (if (= specvar-name (:name spec)) (assoc spec :name replace-value) spec)

         (OR,AND) (-> spec
                      (update :arglist (partial map #(replace-specvar-name-with-value % specvar-name replace-value)))
                      (update :arglist set))

         (USERDEFINED, COMPOUND, TUPLE)
         (-> spec
             (update :arglist (partial map #(replace-specvar-name-with-value % specvar-name replace-value)))
             (update :arglist (partial apply vector)))
         LIST
         (update spec :type #(replace-specvar-name-with-value % specvar-name replace-value))

         spec
         ))


(defn replace-specvars-with-spec [type specvar-name replace-spec]
  (case+ (spec-type type)
         (SPECVAR,UNION,COMPATIBLE) (if (= specvar-name (:name type)) replace-spec type)

         (OR,AND) (-> type
                      (update :arglist (partial map #(replace-specvars-with-spec % specvar-name replace-spec)))
                      (update :arglist set))

         (USERDEFINED, COMPOUND, TUPLE) (-> type
                                            (update :arglist (partial map #(replace-specvars-with-spec % specvar-name replace-spec)))
                                            (update :arglist (partial apply vector)))
         LIST (update type :type #(replace-specvars-with-spec % specvar-name replace-spec))
         type
         ))

(defn replace-union-and-comp-with-specvar [type]
  (case+ (spec-type type)
         (UNION,COMPATIBLE) (->SpecvarSpec (.name type))

         (OR,AND) (-> type
                      (update :arglist (partial map replace-union-and-comp-with-specvar))
                      (update :arglist set))

         (USERDEFINED, COMPOUND, TUPLE) (-> type
                                            (update :arglist (partial map replace-union-and-comp-with-specvar))
                                            (update :arglist (partial apply vector)))

         LIST (update type :type replace-union-and-comp-with-specvar)

         type))

(defn replace-union-and-comp-with-placeholder [type]
  (case+ (spec-type type)
         (UNION,COMPATIBLE) (->PlaceholderSpec type)

         (OR,AND) (-> type
                      (update :arglist (partial map replace-union-and-comp-with-placeholder))
                      (update :arglist set))

         (USERDEFINED, COMPOUND, TUPLE) (-> type
                                            (update :arglist (partial map replace-union-and-comp-with-placeholder))
                                            (update :arglist (partial apply vector)))

         LIST (update type :type replace-union-and-comp-with-placeholder)

         type))


(defn find-specvars [spec]
  (case+ (spec-type spec)
         (SPECVAR,UNION,COMPATIBLE) [spec]
         (OR, AND, COMPOUND, TUPLE) (set (mapcat find-specvars (.arglist spec)))
         USERDEFINED (if (contains? spec :arglist) (set (mapcat find-specvars (:arglist spec))) [])
         LIST (find-specvars (.type spec))
         #{}))


(defn resolve-definition-with-parameters
  "User-defined specs can have parameters and when in use in spec annotations,
  there are values assigned to these parameters. To get the correct definition,
  we have to replace the parameter with their value."
  [{n :name arglist :arglist :as user-def-spec} defs]
  (if (nil? arglist)
      (get defs user-def-spec user-def-spec)
      (let [alias (->> defs
                       keys
                       (filter #(= (:name %) n))
                       (filter #(= (count (:arglist %)) (count arglist)))
                       first)
            definition (get defs alias user-def-spec)
            replace-map (apply hash-map (interleave (map :name (:arglist alias)) arglist))
            result (reduce-kv replace-specvars-with-spec definition replace-map)]
        (if (= OR (spec-type result))
          (update result :arglist set)
          result))))


(defn supertype? [defs parent child]
  (if (= OR (spec-type parent))
    (some #(= child (intersect % child defs)) (:arglist parent))
    (= child (intersect parent child defs))))

(defn has-specvars [spec]
  (or
   (= SPECVAR (spec-type spec))
   (= UNION (spec-type spec))
   (= COMPATIBLE (spec-type spec))
   (some has-specvars (:arglist spec))
   (some->> (:type spec)
            has-specvars)))


(defn replace-specvars-with-any [spec]
  (let [used-specvars (find-specvars spec)
        replace-map (reduce #(assoc %1 (:name %2) (->AnySpec)) {} used-specvars)]
    (reduce-kv replace-specvars-with-spec spec replace-map)))


(defn length-of-list-term [{head :head tail :tail :as list}]
  (cond
    (nil? head) 0
    (nil? tail) 1
    (= VAR (term-type tail)) :inf
    :default
    (let [tail-length (length-of-list-term tail)]
      (if (= :inf tail-length)
        :inf
        (inc tail-length)))))


(defn var-spec? [spec]
  (if (nil? spec)
    false
    (= VAR (spec-type spec))))

(defn any-spec? [spec]
  (if (nil? spec)
    true
    (= ANY (spec-type spec))))


(defn record-type [type]
  (cond
    (nil? type) nil
    (satisfies? spec type) (spec-type spec)
    :else (term-type spec)))

(defn maybe-spec [spec]
  (cond
    (:arglist spec) (->OneOfSpec #{(->VarSpec) (update spec :arglist (partial map (fn [x] (->AnySpec))))})
    (:type spec) (->OneOfSpec #{(->VarSpec) (assoc spec :type (->AnySpec))})
    :else (if (any-spec? spec) spec (->OneOfSpec #{(->VarSpec) spec}))))
