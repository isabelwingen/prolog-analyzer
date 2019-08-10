(ns prolog-analyzer.result-visualizer
  (:require [clojure.data.json :as json]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [prolog-analyzer.utils :as utils]
            [clojure.java.io :refer [writer]]
            [ubergraph.core :as uber]))

(defn- get-indexed-terms [env]
  (uber/attr env :environment :arglist))

(defn- get-error-terms [env]
  (->> env
       utils/get-terms
       (filter #(ru/error-spec? (utils/get-dom-of-term env %)))))

(defn get-value-of-term [env term]
  (str
   (r/to-string (utils/get-dom-of-term env term))
   " ("
   (clojure.string/join "," (map r/to-string (or (uber/attr env term :history) [])))
   ")"))

(defn- to-json-single [env]
  (let [title (uber/attr env :environment :title)
        argument-types (->> env
                            get-indexed-terms
                            (map (partial utils/get-dom-of-term env))
                            (map r/to-string))
        errors (reduce #(assoc %1 (r/to-string %2) (get-value-of-term env %2)) {} (get-error-terms env))]
    {"clause" title "arguments" argument-types "errors" errors}))

(defn to-json [envs file]
  (with-open [wrtr (writer file)]
    (binding [*out* wrtr]
      (json/pprint (map to-json-single envs)))))
