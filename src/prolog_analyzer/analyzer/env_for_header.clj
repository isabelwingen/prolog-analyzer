(ns prolog-analyzer.analyzer.env-for-header
  (:require [prolog-analyzer.analyzer.domain :as dom]
            [prolog-analyzer.utils :as utils]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [ubergraph.core :as uber]))


(defn get-env
  "Calculates an environment from the header terms and the prespec"
  [data {arglist :arglist :as clause} pre-spec]
  (-> (uber/digraph)
      (dom/add-to-dom (apply ru/to-head-tail-list arglist) pre-spec)))
