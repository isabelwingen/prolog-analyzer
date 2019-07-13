(ns prolog-analyzer.analyzer.env-for-header
  (:require [prolog-analyzer.analyzer.domain :as dom]
            [prolog-analyzer.utils :as utils]
            [prolog-analyzer.records :as r]
            [ubergraph.core :as uber]))


(defn get-env [data {arglist :arglist :as clause} pre-spec]
  (-> (uber/digraph)
      (dom/add-to-dom (apply r/to-head-tail-list arglist) pre-spec)))
