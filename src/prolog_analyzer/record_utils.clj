(ns prolog-analyzer.record-utils
  (:require [prolog-analyzer.records :as r]
            [prolog-analyzer.utils :as utils :refer [case+ duocase]]))

(declare intersect)
(declare simplify)
(defn var-spec? [spec]
  (if (nil? spec)
    false
    (= r/VAR (r/safe-spec-type spec "var-spec"))))

(defn any-spec? [spec]
  (if (nil? spec)
    true
    (= r/ANY (r/safe-spec-type spec "any-spec"))))

(defn or-spec? [spec]
  (= r/OR (r/safe-spec-type spec "or-spec?")))

(defn and-spec? [spec]
  (= r/AND (r/safe-spec-type spec "and-spec?")))


(defn error-spec? [spec]
  (or (nil? spec)
      (= r/ERROR (r/safe-spec-type spec "error"))
      (if (contains? spec :type) (error-spec? (.type spec)) false)
      (if (contains? spec :arglist) (some error-spec? (:arglist spec)) false)
      ))

(defn tuple-spec? [spec]
  (= r/TUPLE (r/safe-spec-type spec "tuple-spec?")))

(defn supertype? [defs parent child]
  (if (= r/OR (r/safe-spec-type parent "supertype"))
    (some #(= child (intersect % child defs false)) (:arglist parent))
    (= child (intersect parent child defs false))))

(defn to-or-spec
  "Transforms a bunch of `specs` to a one-of spec."
  [defs & specs]
  (case (count specs)
    0 (r/->ErrorSpec "Cannot build empty one-of")
    1 (first specs)
    (simplify (r/->OneOfSpec (set specs)) defs)))

(declare replace-specvars-with-spec)

(defn resolve-definition-with-parameters
  "User-defined specs can have parameters and when in use in spec annotations,
  there are values assigned to these parameters. To get the correct definition,
  we have to replace the parameter with their value."
  [{n :name arglist :arglist :as user-def-spec} defs]
  (let [res (if (nil? arglist)
              (get defs user-def-spec nil)
              (let [alias (->> defs
                               keys
                               (filter #(= (:name %) n))
                               (filter #(= (count (:arglist %)) (count arglist)))
                               first)
                    definition (get defs alias nil)
                    replace-map (apply hash-map (interleave (map :name (:arglist alias)) arglist))
                    result (reduce-kv replace-specvars-with-spec definition replace-map)]
                (if (= r/OR (r/safe-spec-type result "resolve-definition-with-parameters"))
                  (update result :arglist set)
                  result)))]
    (if (nil? res)
      (if (nil? user-def-spec)
        (throw (Exception. "User-defined spec was nil"))
        (throw (Exception. (str "Could not find definition of userdefined spec " (r/to-string user-def-spec)))))
      res)))

(defn remove-subsets-in-or [{arglist :arglist :as spec} defs]
  (if (= r/OR (r/safe-spec-type spec "remove subsets"))
    (r/->OneOfSpec (reduce
                    (fn [new-arglist type]
                      (if (some #(supertype? defs % type) (remove #(= type %) arglist))
                        new-arglist
                        (conj new-arglist type)))
                    #{}
                    arglist))
    spec))


(defn- same-user-defined-spec? [{name-a :name arglist-a :arglist} {name-b :name arglist-b :arglist}]
  (and (= name-a name-b) (= (count arglist-a) (count arglist-b))))

(defn- same-functors? [left right]
  (= (:functor left) (:functor right)))

(defn- same-arg-number? [left right]
  (= (count (:arglist left)) (count (:arglist right))))

(defn- error-if-empty-arglist [spec]
  (if (and (or-spec? spec) (empty? (:arglist spec))) (r/DISJOINT) spec))

(defn- extract-single [spec]
  (if (and (or-spec? spec) (= 1 (count (:arglist spec)))) (first (:arglist spec)) spec))

(defn- simplify-pair-tuples-in-or [{arglist :arglist :as spec}]
  (let [tuple-arglists (map :arglist arglist)]
    (if (apply = (map last tuple-arglists))
      (r/->TupleSpec [(r/->OneOfSpec (set (map first tuple-arglists))) (last (first tuple-arglists))])
      (if (apply = (map first tuple-arglists))
        (r/->TupleSpec [(first (first tuple-arglists)) (r/->OneOfSpec (set (map last tuple-arglists)))])
        spec))))

(defn extract-singleton-tuples [{arglist :arglist :as spec}]
  (if (and
       (or-spec? spec)
       (not-empty arglist)
       (every? tuple-spec? arglist))
    (if (every? #(= 1 (count (:arglist %))) arglist)
      (r/->TupleSpec [(r/->OneOfSpec (->> arglist
                                          (map :arglist)
                                          (map first)
                                          set))])
      (if (every? #(= 2 (count (:arglist %))) arglist)
        (simplify-pair-tuples-in-or spec)
        spec))
    spec))

(defn remove-error-specs [spec]
  (if (= or-spec? spec)
    (-> spec
        (update :arglist (partial remove #(error-spec? %)))
        (update :arglist set))
    spec))

(defn simplify [spec defs initial?]
  (case+ (r/safe-spec-type spec "simplify")
         r/OR (-> spec
                  (update :arglist (partial mapcat #(if (or-spec? %) (:arglist %) [%])))
                  remove-error-specs
                  (update :arglist (partial map #(simplify % defs initial?)))
                  (update :arglist set)
                  extract-singleton-tuples
                  (remove-subsets-in-or defs)
                  remove-error-specs
                  error-if-empty-arglist
                  extract-single)
         r/AND (reduce #(intersect %1 %2 defs initial?) (r/->AnySpec) (:arglist spec))
         r/LIST (if (error-spec? (:type spec))
                  (:type spec)
                  (update spec :type simplify defs initial?))
         r/TUPLE (if (empty? (:arglist spec))
                   (r/->EmptyListSpec)
                   (-> spec
                       (update :arglist (partial map #(simplify % defs initial?)))
                       (update :arglist (partial apply vector))))
         spec))

(simplify (r/->OneOfSpec #{(r/->TupleSpec [(r/->IntegerSpec)])
                               (r/->TupleSpec [(r/->FloatSpec)])}) {} true)


(defn intersect [spec-a spec-b defs initial?]
  (loop [left spec-a
         right spec-b
         first-time true]
    (let [intersection (duocase [(r/safe-spec-type left "left") (r/safe-spec-type right "right")]
                                [r/ANY :idclol] right

                                [r/VAR r/VAR] right
                                [r/VAR r/ANY] left
                                [r/VAR r/USERDEFINED] :swap
                                [r/VAR r/OR] :swap
                                [r/VAR r/AND] :swap
                                [r/VAR r/PLACEHOLDER] :swap
                                [r/VAR :idclol] (if initial? right nil)

                                [r/GROUND r/LIST] (update right :type #(intersect left % defs initial?))
                                [r/GROUND r/COMPOUND] (if (nil? (:arglist right))
                                                        right
                                                        (update right :arglist (partial map #(intersect left % defs initial?))))
                                [r/GROUND r/TUPLE] (update right :arglist (partial map #(intersect left % defs initial?)))
                                [r/GROUND r/NONVAR] left
                                [r/GROUND r/VAR] (if initial? left nil)
                                [r/GROUND r/ANY] left
                                [r/GROUND r/OR] :swap
                                [r/GROUND r/AND] :swap
                                [r/GROUND r/USERDEFINED] :swap
                                [r/GROUND r/PLACEHOLDER] :wap
                                [r/GROUND :idclol] right

                                [r/NONVAR r/VAR] (if initial? left nil)
                                [r/NONVAR r/ANY] left
                                [r/NONVAR r/USERDEFINED] :swap
                                [r/NONVAR r/PLACEHOLDER] :swap
                                [r/NONVAR :idclol] right

                                [r/EMPTYLIST r/EMPTYLIST] right
                                [r/EMPTYLIST r/ATOMIC] left
                                [r/EMPTYLIST r/LIST] left
                                [r/EMPTYLIST r/COMPOUND] (if (nil? (:functor right))
                                                           left
                                                           nil)
                                [r/EMPTYLIST r/TUPLE] (if (zero? (count (:arglist right)))
                                                        left
                                                        nil)
                                [r/EMPTYLIST :idclol] :swap

                                [r/ATOMIC r/ATOMIC] right
                                [r/ATOMIC r/ATOM] right
                                [r/ATOMIC r/STRING] right
                                [r/ATOMIC r/NUMBER] right
                                [r/ATOMIC r/FLOAT] right
                                [r/ATOMIC r/INTEGER] right
                                [r/ATOMIC r/LIST] (r/->EmptyListSpec)
                                [r/ATOMIC r/EXACT] right
                                [r/ATOMIC :idclol] :swap

                                [r/ATOM r/ATOM] right
                                [r/ATOM r/EXACT] right
                                [r/ATOM :idclol] :swap

                                [r/EXACT r/EXACT] (if (= (:value left) (:value right))
                                                    left
                                                    nil)
                                [r/EXACT :idclol] :swap

                                [r/NUMBER r/NUMBER] right
                                [r/NUMBER r/INTEGER] right
                                [r/NUMBER r/FLOAT] right
                                [r/NUMBER :idclol] :swap

                                [r/INTEGER r/INTEGER] right
                                [r/INTEGER :idclol] :swap

                                [r/FLOAT r/FLOAT] right
                                [r/FLOAT :idclol] :swap

                                [r/STRING r/STRING] right
                                [r/STRING :idclol] :swap

                                [r/LIST r/LIST] (update left :type #(intersect % (:type right) defs initial?))
                                [r/LIST r/COMPOUND] (cond
                                                      (nil? (:functor right)) left
                                                      :else nil)
                                [r/LIST r/TUPLE] (update right :arglist (partial map #(intersect % (:type left) defs initial?)))
                                [r/LIST :idclol] :swap

                                [r/TUPLE r/TUPLE] (if (same-arg-number? left right)
                                                    (update left :arglist (partial map #(intersect %1 %2 defs initial?) (:arglist right)))
                                                    nil)
                                [r/TUPLE r/COMPOUND] (cond
                                                       (nil? (:functor right)) left
                                                       :else nil)
                                [r/TUPLE :idclol] :swap

                                [r/COMPOUND r/COMPOUND] (cond
                                                          (nil? (:functor left)) right
                                                          (nil? (:functor right)) left

                                                          (and (same-functors? left right) (same-arg-number? left right))
                                                          (update left :arglist (partial map #(intersect %1 %2 defs initial?) (:arglist right)))

                                                          :default nil)
                                [r/COMPOUND :idclol] :swap

                                [r/USERDEFINED r/ANY] left
                                [r/USERDEFINED r/USERDEFINED] (cond
                                                                (and
                                                                 (= (:name left) (:name right))
                                                                 (= (:arglist left) (:arglist right) nil))
                                                                left

                                                                (and
                                                                 (= (:name left) (:name right))
                                                                 (= (count (:arglist left)) (count (:arglist right))))
                                                                (update left :arglist (partial map #(intersect %1 %2 defs initial?) (:arglist right)))

                                                                :default
                                                                (intersect (resolve-definition-with-parameters left defs) right defs initial?))
                                [r/USERDEFINED :idclol] (intersect (resolve-definition-with-parameters left defs) right defs initial?)

                                [r/OR r/OR] (let [new-arglist (set (for [a (:arglist left)
                                                                         b (:arglist right)]
                                                                     (intersect a b defs initial?)))]
                                              (assoc left :arglist new-arglist))
                                [r/OR r/AND] :swap
                                [r/OR :idclol] (update left :arglist (partial map #(intersect % right defs initial?)))

                                [r/AND r/AND] (reduce #(intersect %1 %2 defs initial?) (r/->AnySpec) (concat (:arglist left) (:arglist right)))
                                [r/AND r/OR] (let [new-left (reduce #(intersect %1 %2 defs initial?) (r/->AnySpec) (:arglist left))
                                                   new-arglist (set (for [a (:arglist right)]
                                                                      (intersect new-left a defs initial?)))]
                                               (assoc right :arglist new-arglist))
                                [r/AND :idclol] (reduce #(intersect %1 %2 defs initial?) right (:arglist left))

                                [r/PLACEHOLDER r/ANY] left
                                [r/PLACEHOLDER :idclol] (assoc left :alias right)

                                [r/UNION :idclol] right
                                [r/COMPATIBLE :idclol] right

                                [r/ERROR :idclol] left


                                nil)
          to-error-spec (fn [i] (if (or (nil? i) (= :swap i)) (r/DISJOINT left right) i))]
      (if (and first-time (= :swap intersection))
        (recur right left false)
        (simplify (to-error-spec intersection) defs initial?)))))

(defn intersect* [initial? defs & specs]
  (simplify (reduce #(intersect %1 %2 defs initial?) (r/->AnySpec) specs) defs initial?))

(defn to-tuple-spec
  "Transforms a bunch of `specs` to a tuple spec."
  [& specs]
  (if (empty? specs)
    (r/->TupleSpec [])
    (r/->TupleSpec specs)))

(defn to-head-tail-list [& terms]
  (if (empty? terms)
    (r/->EmptyListTerm)
    (r/->ListTerm (first terms) (apply to-head-tail-list (rest terms)))))

(defn replace-specvar-name-with-value [spec specvar-name replace-value]
  (case+ (r/safe-spec-type spec "replace-with-value")
         (r/SPECVAR,r/UNION,r/COMPATIBLE)
         (if (= specvar-name (:name spec)) (assoc spec :name replace-value) spec)

         (r/OR,r/AND) (-> spec
                      (update :arglist (partial map #(replace-specvar-name-with-value % specvar-name replace-value)))
                      (update :arglist set))

         (r/USERDEFINED, r/COMPOUND, r/TUPLE)
         (-> spec
             (update :arglist (partial map #(replace-specvar-name-with-value % specvar-name replace-value)))
             (update :arglist (partial apply vector)))
         r/LIST
         (update spec :type #(replace-specvar-name-with-value % specvar-name replace-value))

         spec
         ))


(defn replace-specvars-with-spec [type specvar-name replace-spec]
  (case+ (r/safe-spec-type type "replace with spec")
         (r/SPECVAR,r/UNION,r/COMPATIBLE) (if (= specvar-name (:name type)) replace-spec type)

         (r/OR,r/AND) (-> type
                      (update :arglist (partial map #(replace-specvars-with-spec % specvar-name replace-spec)))
                      (update :arglist set))

         (r/USERDEFINED, r/COMPOUND, r/TUPLE) (-> type
                                            (update :arglist (partial map #(replace-specvars-with-spec % specvar-name replace-spec)))
                                            (update :arglist (partial apply vector)))
         r/LIST (update type :type #(replace-specvars-with-spec % specvar-name replace-spec))
         type
         ))

(defn replace-union-and-comp-with-placeholder [type]
  (case+ (r/safe-spec-type type "replace union")
         (r/UNION,r/COMPATIBLE) (r/->PlaceholderSpec type)

         (r/OR,r/AND) (-> type
                      (update :arglist (partial map replace-union-and-comp-with-placeholder))
                      (update :arglist set))

         (r/USERDEFINED, r/COMPOUND, r/TUPLE) (-> type
                                            (update :arglist (partial map replace-union-and-comp-with-placeholder))
                                            (update :arglist (partial apply vector)))

         r/LIST (update type :type replace-union-and-comp-with-placeholder)

         type))


(defn find-specvars [spec]
  (case+ (r/safe-spec-type spec "Find specvars")
         (r/SPECVAR,r/UNION,r/COMPATIBLE) [spec]
         (r/OR, r/AND, r/COMPOUND, r/TUPLE) (set (mapcat find-specvars (.arglist spec)))
         r/USERDEFINED (if (contains? spec :arglist) (set (mapcat find-specvars (:arglist spec))) [])
         r/LIST (find-specvars (.type spec))
         #{}))



(defn has-specvars [spec]
  (or
   (= r/SPECVAR (r/safe-spec-type spec "has specvar"))
   (= r/UNION (r/safe-spec-type spec "has specvar"))
   (= r/COMPATIBLE (r/safe-spec-type spec "has specvar"))
   (some has-specvars (:arglist spec))
   (some->> (:type spec)
            has-specvars)))


(defn replace-specvars-with-any [spec]
  (let [used-specvars (find-specvars spec)
        replace-map (reduce #(assoc %1 (:name %2) (r/->AnySpec)) {} used-specvars)]
    (reduce-kv replace-specvars-with-spec spec replace-map)))


(defn length-of-list-term [{head :head tail :tail :as list}]
  (cond
    (nil? head) 0
    (nil? tail) 1
    (= r/VAR (r/term-type tail)) :inf
    :default
    (let [tail-length (length-of-list-term tail)]
      (if (= :inf tail-length)
        :inf
        (inc tail-length)))))


(defn nonvar-term? [term]
  (not= (r/term-type term) r/VAR))

(defn maybe-spec [spec]
  (cond
    (:arglist spec) (r/->OneOfSpec #{(r/->VarSpec) (update spec :arglist (partial map (fn [x] (r/->AnySpec))))})
    (:type spec) (r/->OneOfSpec #{(r/->VarSpec) (assoc spec :type (r/->AnySpec))})
    :else (if (any-spec? spec) spec (r/->OneOfSpec #{(r/->VarSpec) spec}))))

(defn list-term? [term]
  (= r/LIST (r/term-type term)))

(defn compound-term? [term]
  (= r/COMPOUND (r/term-type term)))

(defn single-term? [term]
  (and (not (list-term? term))
       (not (compound-term? term))))

(defn head [list-term]
  (:head list-term))

(defn tail [list-term]
  (:tail list-term))

(defn arg [compound-or-tuple n]
  (nth (:arglist compound-or-tuple) n))

(defn compound-with-anys [fun n]
  (r/->CompoundSpec fun (apply vector (repeat n (r/->AnySpec)))))

(defn tuple-with-anys [n]
  (r/->TupleSpec (apply vector (repeat n (r/->AnySpec)))))

(defn list-with-anys []
  (r/->ListSpec (r/->AnySpec)))

(defn non-empty-intersection [spec1 spec2 defs initial?]
  (not (error-spec? (intersect spec1 spec2 defs initial?))))

(defn replace-var-with-any [spec]
  (case+ (r/spec-type spec)
         r/VAR (r/->AnySpec)
         r/LIST (update spec :type replace-var-with-any)
         (r/TUPLE, r/COMPOUND) (update spec :arglist #(->> %
                                                           (map replace-var-with-any)
                                                           (apply vector)))
         (r/USERDEFINED) (if (nil? (:arglist spec)) spec (update spec :arglist #(->> % (map replace-var-with-any) (apply vector))))
         spec))
