(ns prolog-analyzer.analyzer.domain
  (:require [prolog-analyzer.utils :as utils :refer [case+]]
            [prolog-analyzer.records :as r]
            [ubergraph.core :as uber]))

(declare add-to-dom)

(defn remove-nested [spec]
  (case+ (r/spec-type spec)
         (r/TUPLE, r/COMPOUND) (update spec :arglist #(repeat (count %) (r/->AnySpec)))
         r/LIST (assoc spec :type (r/->AnySpec))
         r/USERDEFINED (if (nil? (:arglist spec)) spec (update spec :arglist #(repeat (count %) (r/->AnySpec))))
         (r/OR, r/AND) (update spec :arglist remove-nested)
         spec))

(defmulti next-steps (fn [term spec] [(if (r/nonvar-term? term) :nonvar :var)
                                     (case+ (r/spec-type spec)
                                            r/TUPLE :tuple
                                            r/COMPOUND :compound
                                            r/LIST :list
                                            r/USERDEFINED :userdef
                                            r/OR :or
                                            r/AND :and
                                            :unnested)]))
(def DEFAULT-NEXT-STEPS {:steps [] :edges []})

(defn- next-step-answer [steps edges]
  {:steps steps :edges edges})

(defmethod  next-steps [:nonvar :tuple] [term spec]
  (if (r/list-term? term)
    (next-step-answer [[(r/head term) (first (:arglist spec))]
                       [(r/tail term) (update spec :arglist rest)]]
                      [[(r/head term) term {:relation :is-head}]
                       [(r/tail term) term {:relation :is-tail}]])
    DEFAULT-NEXT-STEPS))
(defmethod  next-steps [:nonvar :compound] [term spec] DEFAULT-NEXT-STEPS)
(defmethod  next-steps [:nonvar :list] [term spec] DEFAULT-NEXT-STEPS)
(defmethod  next-steps [:nonvar :userdef] [term spec] DEFAULT-NEXT-STEPS)
(defmethod  next-steps [:nonvar :or] [term spec] DEFAULT-NEXT-STEPS)
(defmethod  next-steps [:nonvar :and] [term spec] DEFAULT-NEXT-STEPS)
(defmethod  next-steps [:nonvar :unnested] [term spec] DEFAULT-NEXT-STEPS)

(defmethod  next-steps [:var :tuple] [term spec] DEFAULT-NEXT-STEPS)
(defmethod  next-steps [:var :compound] [term spec] DEFAULT-NEXT-STEPS)
(defmethod  next-steps [:var :list] [term spec] DEFAULT-NEXT-STEPS)
(defmethod  next-steps [:var :userdef] [term spec] DEFAULT-NEXT-STEPS)
(defmethod  next-steps [:var :or] [term spec] DEFAULT-NEXT-STEPS)
(defmethod  next-steps [:var :and] [term spec] DEFAULT-NEXT-STEPS)
(defmethod  next-steps [:var :unnested] [term spec] DEFAULT-NEXT-STEPS)


(defn process-next-steps [env term spec]
  (let [{steps :steps edges :edges} (next-steps term spec)]
    (reduce
     #(apply add-to-dom %1 %2)
     (apply uber/add-edges env edges)
     steps)))

(defn add-to-dom [env term spec]
  (-> env
      (uber/add-nodes term)
      (utils/update-attr term :dom #(if (nil? %1) [%2] (conj %1 %2)) (remove-nested spec))
      (process-next-steps term spec)
      ))
