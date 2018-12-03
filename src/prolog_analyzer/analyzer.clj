(ns prolog-analyzer.analyzer
  (:require
   [prolog-analyzer.parser :refer [process-prolog-file process-prolog-snippets]] ;; used only during development
   ))


(defmulti valid-helper #(:spec (first %)))

(defn valid [spec arg]
  (valid-helper [spec arg]))


(defmethod valid-helper :number [[spec arg]]
  (or (= :number (:type arg))
      (= :integer (:type arg))
      (= :float (:type arg))
      ))

(defmethod valid-helper :atomic [[spec arg]]
  (or
   (= :atomic (:type arg))
   (= :atom (:type arg))
   (valid {:spec :number} arg)
   ))

(defmethod valid-helper :integer [[spec arg]]
  (= :integer (:type arg)))

(defmethod valid-helper :float [[spec arg]]
  (= :float (:type arg)))

(defmethod valid-helper :atom [[spec arg]]
  (= :atom (:type arg)))

(defmethod valid-helper :exact [[{value :value} arg]]
  (and
   (= :atom (:type arg))
   (= value (:term arg))))

(defmethod valid-helper :any [[spec arg]]
  (if (nil? (:name spec))
    true))

(defmethod valid-helper :ground [[spec arg]]
  (or
   (= :ground (:type arg))
   (valid {:spec :atomic} arg)
   (if (= (:type arg) :compound)
     (every? true? (map (partial valid {:spec :ground}) (:arglist arg)))
     false)))

(defmethod valid-helper :nonvar [[spec arg]]
  (and
   (not= :var (:type arg))
   (not= :anon_var (:type arg))))

(defmethod valid-helper :var [[spec arg]]
  (or
   (= :var (:type arg))
   (= :anon_var (:type arg))))

(defmethod valid-helper :list [[{inner-type :type} arg]]
  (if (= :head-tail-list (:type arg))
    false
    (every? (partial valid inner-type) (:arglist arg))))

(defmethod valid-helper :tuple [[spec arg]]
  (if (= :list (:type arg))
    (every? true? (map valid (:arglist spec) (:arglist arg)))
    false))

(defmethod valid-helper :one_of [[spec arg]]
  (not (not-any? true? (for [x (:arglist spec)]
                         (valid x arg)))))

(defmethod valid-helper :and [[spec arg]]
  (every? true (for [x (:arglist spec)]
                 (valid x arg))))

(defmethod valid-helper :compound [[{spec-functor :functor spec-arglist :arglist} {arg-functor :functor arg-arglist :arglist}]]
  (and (= spec-functor arg-functor)
       (every? true? (map valid spec-arglist arg-arglist))))




(defmethod valid-helper :default [[spec arg]]
  (println (str "default: " spec)))


(process-prolog-snippets ":- declare_spec(tree(any(_))).\n:-define_spec(tree(any(X)),one_of([compound(node(tree(any(X)),any(X),tree(any(X)))),atom(empty)])).\n:- spec_pre(foo/1,[tree(int)]).")

(valid {:spec :one_of :arglist [{:spec :float} {:spec :atom}]} {:value 1 :type :integer})
