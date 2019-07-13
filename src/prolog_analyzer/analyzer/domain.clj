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

(defn add-to-dom [env term spec]
  (-> env
      (uber/add-nodes term)
      (utils/update-attr term :dom #(if (nil? %1) [%2] (conj %1 %2)) (remove-nested spec))
      ))
