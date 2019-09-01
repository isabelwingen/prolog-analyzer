(ns prolog-analyzer.parser.create-post-specs
  (:require [prolog-analyzer.utils :as utils :refer [case+]]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [clojure.tools.logging :as log]
            [prolog-analyzer.state :as state]
            [clojure.spec.alpha :as s]
            [prolog-analyzer.specs :as specs]
            [orchestra.spec.test :as stest]
            [orchestra.core :refer [defn-spec]]
            ))

(defn- create-any-conclusion [n]
  (vec (map #(hash-map :id % :type (r/->AnySpec)) (range 0 n))))



(defn create-any-post-spec [arity]
  {:guard [] :conclusion (vector (create-any-conclusion arity))})



(defn add-any-post-specs
  "If there are no post-specs, add one"
  [data]
  (loop [pred-ids (utils/get-pred-identities data)
         result data]
    (if-let [[module pred-name arity :as pred-id] (first pred-ids)]
      (if (nil? (utils/get-post-specs pred-id result))
        (recur (rest pred-ids) (assoc-in result [:post-specs pred-id] [(create-any-post-spec arity)]))
        (recur (rest pred-ids) result))
      result)))
