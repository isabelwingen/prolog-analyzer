(ns prolog-analyzer.analyzer.env-for-header
  (:require [prolog-analyzer.analyzer.domain :as dom]
            [prolog-analyzer.utils :as utils]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [clojure.pprint :refer [pprint]]
            [ubergraph.core :as uber]))

(def INITIAL true)


(defn remove-first-element-in-dom [v]
  ((comp (partial apply vector) rest) v))

(defn calculate-new-dom [env term]
  (let [spec (apply ru/intersect* INITIAL {} (utils/get-dom-of-term env term nil))
        new-env (-> env
                    (uber/remove-attr term dom/DOM)
                    (dom/add-to-dom term spec)
                    )
        new-dom (seq (utils/get-dom-of-term new-env term #{}))]
    (assert (= 1 (count new-dom)) (str "new-dom is wrong " (count new-dom) (apply vector (map r/to-string new-dom))" " (r/to-string term)))
    new-env))

(defn post-process-env [env]
  (loop [res env]
    (pprint (utils/env->map res))
    (let [p (dom/get-terms-with-multiple-doms res)]
      (if (empty? p)
        res
        (recur (reduce calculate-new-dom res p))))))

(defn get-env
  "Calculates an environment from the header terms and the prespec"
  [data {arglist :arglist :as clause} pre-spec]
  (-> (uber/digraph)
      (dom/add-to-dom (apply ru/to-head-tail-list arglist) pre-spec)
      post-process-env
      ))
