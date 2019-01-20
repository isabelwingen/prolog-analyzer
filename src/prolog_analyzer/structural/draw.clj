(ns prolog-analyzer.structural.draw
  (:require [quil.core :as q]
            [ubergraph.core :as uber]
            [prolog-analyzer.utils :as utils]
            [prolog-analyzer.structural.predicate-relations :as rel]))


(defrecord Circle [x y radius text color])

(def pred-color [230 130 130])
(def clause-color [130 130 230])
(def pred-to-clause-color [200 0 0])
(def clause-to-pred-color [0 0 200])

(defn init-circles []
    (loop [nodes (uber/nodes @rel/graph)
           y-pred 200
           y-clause 200]
      (let [node (first nodes)
            [color x y new-y-pred new-y-clause] (if (utils/pred-key? node)
                                                  [pred-color 100 y-pred (+ 200 y-pred) y-clause]
                                                  [clause-color 900 y-clause y-pred (+ 200 y-clause)])]
        (if (not (nil? node))
          (do 
            (swap!
             rel/graph
             utils/assoc-attr
             node
             :circle
             (Circle. x y 80 (str node) color))
            (recur (rest nodes) new-y-pred new-y-clause))))))


(comment 
  (defn init-circles []
    (let [nodes (uber/nodes @rel/graph)
          node-count (count nodes)
          r 450
          center-x 600
          center-y 600
          base-phi (if (zero? node-count) 0 (quot 360 node-count))
          ]
      (doseq [i (range 0 (count nodes))
              :let [node (nth nodes i)
                    phi (* i base-phi)
                    x (* r (q/cos (q/radians phi)))
                    y (* r (q/sin (q/radians phi)))
                    color (if (= 4 (count node)) clause-color pred-color)]]
        (swap!
         rel/graph
         utils/assoc-attr
         node
         :circle
         (Circle. (+ center-x x) (+ center-y y) 80 (str node) color))
        ))))

(defn get-center-of-node [node]
  ((juxt :x :y) (uber/attr @rel/graph node :circle)))

(defn get-circle-of-node [node]
  (uber/attr @rel/graph node :circle))

(defn draw-normal-circle [circle]
  (let [x (:x circle)
        y (:y circle)
        width (* 2 (:radius circle))
        color (:color circle)]
    (apply q/fill color)
    (q/ellipse x y width width)
    (q/fill 50)
    (q/text-align :center :center)
    (q/text (:text circle) x y)))

(defn draw-highlight-circle [circle]
  (let [x (:x circle)
        y (:y circle)
        width (+ 5 (* 2 (:radius circle)))
        color (map (partial + 20) (:color circle))]
    (apply q/fill color)
    (q/ellipse x y width width)
    (q/fill 10)
    (q/text-align :center :center)
    (q/text (:text circle) x y)))

(defn move-circles [x y]
  (doseq [node (uber/nodes @rel/graph)]
    (swap! rel/graph utils/update-attr node :circle #(-> %
                                                    (update :x (partial + x))
                                                    (update :y (partial + y))))))

(defn mouse-in-circle? [circle]
  (let [x-diff (- (:x circle) (q/mouse-x))
        y-diff (- (:y circle) (q/mouse-y))
        distance-sqr (+ (* x-diff x-diff) (* y-diff y-diff))
        radius-sqr (* (:radius circle) (:radius circle))]
    (< distance-sqr radius-sqr)))


(defn draw []
  (q/background 100)
  (let [t (q/frame-count)]
    (q/text (str "mouse-x " (q/mouse-x)) 10 10)
    (q/text (str "mouse-y " (q/mouse-y)) 10 30)
    (doseq [edge (uber/edges @rel/graph)
            :let [src (uber/src edge)
                  dest (uber/dest edge)
                  color (if (utils/pred-key? src) pred-to-clause-color clause-to-pred-color)
                  p1 (get-center-of-node dest)
                  p2 (map (partial + 20) (get-center-of-node src))
                  p3 (map (partial + 10) p2)]]
      (apply q/fill color)
      (apply q/triangle (concat p1 p2 p3))
      )
    (doseq [c (map get-circle-of-node (uber/nodes @rel/graph))]
      (if (mouse-in-circle? c)
        (draw-highlight-circle c)
        (draw-normal-circle c))
      (q/fill 255))))

(defn setup []
  (q/frame-rate 60)
  (q/background 100))

(defn mouse-dragged []
  (let [x-diff (- (q/mouse-x) (q/pmouse-x))
        y-diff (- (q/mouse-y) (q/pmouse-y))]
    (move-circles x-diff y-diff)))

(defn draw-relationships [data]
  (rel/create-graph data)
  (init-circles)
  (q/defsketch trigonometry
      :size [1200 1200]
      :setup setup
      :draw draw
      :mouse-dragged mouse-dragged))





