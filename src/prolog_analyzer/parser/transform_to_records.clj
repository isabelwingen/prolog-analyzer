(ns prolog-analyzer.parser.transform-to-records
  (:require [prolog-analyzer.records :as r]
            [prolog-analyzer.utils :as utils]))

(defn- transform-arglist [singletons args]
  (->> args
       (map (partial r/map-to-term singletons))
       (apply vector)))

(declare transform-body)

(defn- transform-body-elements [singletons {goal-name :goal arglist :arglist :as goal}]
  (case goal-name
    (:or, :if, :not) (-> goal
                         (update :arglist (partial map (partial transform-body singletons)))
                         (assoc :module :built-in))
    (update goal :arglist (partial transform-arglist singletons))))

(defn- transform-body [singletons body]
  (map (partial transform-body-elements singletons) body))


(defn transform-args-to-term-records [data]
  (reduce (fn [data [pred-id clause-number]]
            (let [singletons (get-in data [:singletons pred-id clause-number])]
              (-> data
                  (update-in [:preds pred-id clause-number :arglist] (partial transform-arglist singletons))
                  (update-in [:preds pred-id clause-number :body] (partial transform-body singletons))
                  ))
            )
          data
          (utils/get-clause-identities data)))
