(ns prolog-analyzer.analyzer.validator
  (:require [prolog-analyzer.utils :as utils]))

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

(defmethod valid-helper :list [[{inner-type :type :as spec} arg]]
  (if (utils/empty-list? arg)
    true
    (and (valid inner-type (:head arg)) (valid spec (:tail arg)))))

(defmethod valid-helper :tuple [[{spec-arglist :arglist :as spec} arg]]
  (cond
    (and (empty? spec-arglist) (utils/empty-list? arg)) true
    (= :list (:type arg)) (and (valid (first spec-arglist) (:head arg)) (valid (update spec :arglist rest) (:tail arg)))
    :else false))

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

