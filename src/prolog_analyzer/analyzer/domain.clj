(ns prolog-analyzer.analyzer.domain
  (:require [prolog-analyzer.utils :as utils :refer [case+]]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [ubergraph.core :as uber]))

(declare add-to-dom)

(def DOM :dom)
(def HIST :history)


(defmulti next-steps (fn [term spec] [(if (ru/nonvar-term? term) :nonvar :var)
                                     (case+ (r/spec-type spec)
                                            r/TUPLE :tuple
                                            r/COMPOUND :compound
                                            r/LIST :list
                                            r/USERDEFINED :userdef
                                            r/OR :or
                                            r/AND :and
                                            :unnested)]))
(defn- split-spec-for-list [spec]
  (case+ (r/spec-type spec)
         r/LIST [(:type spec) spec]
         r/TUPLE [(first (:arglist spec)) (-> spec
                                              (update :arglist rest)
                                              (update :arglist (partial apply vector)))]
         [spec spec]))


(defn create-or-part-specs [term spec]
  (let [head (ru/head term)
        tail (ru/tail term)
        [head-spec tail-spec :as p] (->> spec
                                   :arglist
                                   (map split-spec-for-list)
                                   (apply map vector)
                                   (map set)
                                   (map r/->OneOfSpec))]
    (if (nil? head)
      []
      (if (nil? tail)
        [[head head-spec]]
        [[head head-spec]
         [tail tail-spec]]))))

(def DEFAULT-NEXT-STEPS {:steps [] :edges []})

(defn- next-step-answer [steps edges]
  {:steps steps :edges edges})

(defmethod next-steps [:nonvar :tuple] [term spec]
  (if (ru/list-term? term)
    (next-step-answer
     [[(ru/head term) (first (:arglist spec))]
      [(ru/tail term) (update spec :arglist rest)]]
     [[(ru/head term) term {:relation :is-head :uuid (gensym)}]
      [(ru/tail term) term {:relation :is-tail :uuid (gensym)}]])
    DEFAULT-NEXT-STEPS))

(defmethod  next-steps [:nonvar :compound] [term spec]
  (if (ru/compound-term? term)
    (next-step-answer
     (apply vector (map vector (:arglist term) (:arglist spec)))
     (map-indexed #(vector %2 term {:relation :arg-at-pos :pos %1 :uuid (gensym)}) (:arglist term)))
    DEFAULT-NEXT-STEPS)
  )

(defmethod  next-steps [:nonvar :list] [term spec]
  (if (ru/list-term? term)
    (next-step-answer
     [[(ru/head term) (:type spec)]
      [(ru/tail term) spec]]
     [[(ru/head term) term {:relation :is-head :uuid (gensym)}]
      [(ru/tail term) term {:relation :is-tail :uuid (gensym)}]])
    DEFAULT-NEXT-STEPS))

(defmethod  next-steps [:nonvar :userdef] [term spec] DEFAULT-NEXT-STEPS) ;;TODO

(defmethod  next-steps [:nonvar :or] [term spec]
  (next-step-answer
   (if (ru/list-term? term)
     (create-or-part-specs term spec);;TODO: Add case for Compounds
     [])
   []))

(defmethod  next-steps [:nonvar :and] [term spec]
  (next-step-answer
   (let [[f & r] (:arglist spec)]
     (if (empty? r)
       [[term f]]
       [[term f] [term (assoc spec :arglist r)]]))
   []))

(defmethod  next-steps [:nonvar :unnested] [term spec] DEFAULT-NEXT-STEPS)

(defmethod  next-steps [:var :tuple] [term spec] DEFAULT-NEXT-STEPS)
(defmethod  next-steps [:var :compound] [term spec] DEFAULT-NEXT-STEPS)
(defmethod  next-steps [:var :list] [term spec] DEFAULT-NEXT-STEPS)
(defmethod  next-steps [:var :userdef] [term spec] DEFAULT-NEXT-STEPS)
(defmethod  next-steps [:var :or] [term spec] DEFAULT-NEXT-STEPS)

(defmethod  next-steps [:var :and] [term spec]
  (next-step-answer
   (let [[f & r] (:arglist spec)]
     (if (empty? r)
       [[term f]]
       [[term f] [term (assoc spec :arglist r)]]))
   []))

(defmethod  next-steps [:var :unnested] [term spec] DEFAULT-NEXT-STEPS)


(defn process-next-steps [env term spec]
  (let [{steps :steps edges :edges} (next-steps term spec)]
    (reduce
     #(apply add-to-dom %1 %2)
     (apply uber/add-edges env edges)
     steps)))


(defn process-edges [env term])

(defn add-to-dom [env term spec]
  (if (uber/has-node? env term)
    (-> env
        (utils/update-attr term DOM #(set (conj %1 %2)) spec)
        (utils/update-attr term HIST #(conj %1 %2) spec)
        (process-next-steps term spec)
        )
    (-> env
        (uber/add-nodes term)
        (add-to-dom term (r/initial-spec term))
        (add-to-dom term spec))))

(defn get-terms-with-multiple-doms [env]
  (->> env
       utils/get-terms
       (filter #(> (count (utils/get-dom-of-term env % [])) 1))))
