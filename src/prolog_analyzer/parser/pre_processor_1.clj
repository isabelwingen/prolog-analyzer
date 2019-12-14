(ns prolog-analyzer.parser.pre-processor-1
  (:require [clojure.java.io :as io]
            [clojure.set :refer [rename-keys]]
            [clojure.tools.logging :as log]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.parser.transform-spec :refer [transform-spec]]))

;; helper
(defn- create-singleton-maps [clauses]
  (reduce-kv
   (fn [m index {module :module name :name arity :arity singletons :singletons}]
     (assoc-in m [[module name arity] index] singletons))
   {}
   clauses))


(defn- remove-built-ins [singleton-map]
  (let [wrong-keys (->> singleton-map
                        keys
                        (filter #(contains? #{"builtins", "annotations"} (first %))))]
    (apply dissoc singleton-map wrong-keys)))

(defmulti ^{:private true} specs-to-map :goal)

(defmethod specs-to-map :default [{:keys [module functor arity arglist]}]
  {[module functor arity] (vector (map transform-spec arglist))})


(defn create-guard [guard]
  (->> guard
       (map #(update % :type transform-spec))
       (apply vector)))

(defn create-conclusion [conc]
  (->> conc
       (map create-guard)
       (apply vector)))

(defmethod specs-to-map :spec-post [{:keys [module functor arity guard conclusion]}]
  {[module functor arity]
   [(r/->Postspec (create-guard guard) (create-conclusion conclusion))]})


(defn- apply-function-on-values [func in-map]
  (reduce-kv #(assoc %1 %2 (func %3)) {} in-map))

(defn- group-by-and-apply [data f g]
  (->> data
       (group-by f)
       (apply-function-on-values g)
       ))


;; order

(defn- order-define-specs [define-specs]
  (reduce (fn [m {alias :alias def :definition}] (assoc m (transform-spec alias) (transform-spec def))) {} define-specs))


(defn- clean-up-spec-definitions [central-map]
  (let [specs (:define-spec central-map)]
    (-> central-map
        (dissoc :define-spec)
        (dissoc :declare-spec)
        (assoc :specs specs))))

(defn- order-specs [specs]
  (->> specs
       (map specs-to-map)
       (apply merge-with into)
       (reduce-kv (fn [m keys v] (update m keys #(into % v))) {})
       ))

(defn order-post-specs [specs]
  (->> specs
       (map specs-to-map)
       (apply merge-with into)
       (reduce-kv (fn [m keys v] (update m keys #(into % v))) {})
       ))

(defn order-preds [preds]
  (->> preds
       (group-by (juxt :module :name :arity))
       (reduce-kv (fn [m k v] (assoc m k (->> v
                                              (map #(-> % (dissoc :module) (dissoc :name) (dissoc :arity)))
                                              (interleave (range))
                                              (apply hash-map)))) {})))



(defn order-modules [modules]
  (reduce
   (fn [res {path :path module :module partial :partial}]
     (if partial
       (assoc res (.getAbsolutePath (io/file path module)) module)
       (assoc res path module)))
   {}
   modules))


(defn- order-singletons [raw]
  (->> raw
       (group-by (juxt :module :name :arity))
       vals
       (map create-singleton-maps)
       (apply merge-with into)
       remove-built-ins
       ))

(defn order-imports [{module-mapping :module module-imports :use-module :as data}]
  (let [imports (map #(if (:non-lib %) (assoc % :path (.getAbsolutePath (io/file (:source-path %) (:non-lib %)))) %) module-imports)
        non-libs-all (->> imports
                          (remove :lib)
                          (filter #(= :all (:preds %)))
                          (map #(assoc % :module (get module-mapping (:path %))))
                          (map #(select-keys % [:module :in]))
                          (reduce #(assoc-in %1 [(:in %2) (:module %2)] :all) {}))
        non-libs (->> imports
                      (remove :lib)
                      (remove #(= :all (:preds %)))
                      (map #(assoc % :module (get module-mapping (:path %))))
                      (map #(select-keys % [:module :preds :in]))
                      (reduce #(assoc-in %1 [(:in %2) (:module %2)] (->> %2
                                                                         :preds
                                                                         set)) {}))
        libs (->> imports
                  (filter :lib)
                  (remove #(= :all (:preds %)))
                  (reduce #(assoc-in %1 [(:in %2) (:lib %2)] (->> %2
                                                                  :preds
                                                                  set)) {}))
        libs-all (->> imports
                      (filter :lib)
                      (filter #(= :all (:preds %)))
                      (reduce #(assoc-in %1 [(:in %2) (:lib %2)] :all) {}))]
    (-> data
        (dissoc :use-module)
        (assoc :imports (merge-with into non-libs-all non-libs libs-all libs)))))


;; main
(defn format-and-clean-up [data]
  (log/info "Start formatting of edn")
  (let [p (-> data
              (group-by-and-apply :type (partial map :content))
              (update :define-spec order-define-specs)
              clean-up-spec-definitions
              (update :pre-spec order-specs)
              (update :post-spec order-post-specs)
              (update :pred order-preds)
              (update :module order-modules)
              (update :singletons order-singletons)
              order-imports
              (rename-keys {:pre-spec :pre-specs :post-spec :post-specs :pred :preds})
              )]
    (log/info "Done formatting of edn")
    p))
