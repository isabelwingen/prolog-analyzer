(ns prolog-analyzer.parser.add-userdefs
  (:require [prolog-analyzer.records :as r]
            [prolog-analyzer.state :as state]
            [prolog-analyzer.record-utils :as ru]
            [prolog-analyzer.utils :refer [case+]]))

(defmulti create-grounded-version (fn [_ i] i))
(defmethod create-grounded-version true [spec _]
  (case+ (r/spec-type spec)
         r/USERDEFINED (ru/grounded-version spec true)
         r/VAR (r/->GroundSpec)
         (r/OR, r/AND) (-> spec
                           (update :arglist (partial map #(create-grounded-version % true)))
                           (update :arglist set))
         r/LIST (update spec :type create-grounded-version true)
         (r/TUPLE, r/COMPOUND) (-> spec
                                   (update :arglist (partial map #(create-grounded-version % true)))
                                   (update :arglist vec))
         spec))

(defmethod create-grounded-version false [spec _]
  (case+ (r/spec-type spec)
         r/USERDEFINED (ru/grounded-version spec false)
         r/VAR (r/->ErrorSpec (str "Could not ground userdefined spec " (r/to-string spec)))
         (r/OR, r/AND) (-> spec
                           (update :arglist (partial map #(create-grounded-version % false)))
                           (update :arglist set))
         r/LIST (update spec :type create-grounded-version false)
         (r/TUPLE, r/COMPOUND) (-> spec
                                   (update :arglist (partial map #(create-grounded-version % false)))
                                   (update :arglist vec))
         spec))

(defn add-grounded-userdefs [data]
  (doseq [p (keys @state/user-typedefs)
          initial [true false]
          :let [v (get @state/user-typedefs p)]]
    (swap! state/user-typedefs assoc (create-grounded-version p initial) (create-grounded-version v initial)))
  data)


(defn assert-spec-defs [data]
  (reset! state/user-typedefs (:specs data))
  data)

(defn do-it [data]
  (-> data
      assert-spec-defs
      add-grounded-userdefs))
