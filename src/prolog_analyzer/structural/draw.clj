(ns prolog-analyzer.structural.draw
  (:require [quil.core :as q]
            [prolog-analyzer.structural.predicate-relations :as rel]))

(defrecord Circle [x y radius text])

(def circles (atom []))
(def focus (atom 0))

(defn get-focused-circle []
  (nth @circles @focus))

(defn init-circles []
  (reset! circles [])
  (swap! circles conj (Circle. 100 150 50 "A"))
  (swap! circles conj (Circle. 250 150 50 "B"))
  (swap! circles conj (Circle. 400 150 50 "C"))
  (swap! circles conj (Circle. 550 150 50 "D"))
  (swap! circles conj (Circle. 700 150 50 "E"))
  (reset! focus 0)
  )


(defn draw-normal-circle [circle]
  (q/fill 66 188 224)
  (let [x (:x circle)
        y (:y circle)
        width (* 2 (:radius circle))]
    (q/ellipse x y width width)
    (q/fill 50)
    (q/text-align :center :center)
    (q/text (:text circle) x y)))

(defn draw-highlight-circle [circle]
  (q/fill 66 150 224)
  (let [x (:x circle)
        y (:y circle)
        width (+ 5 (* 2 (:radius circle)))]
    (q/ellipse x y width width)
    (q/fill 10)
    (q/text-align :center :center)
    (q/text (:text circle) x y)))

(defn move-circles [x y]
  (swap! circles (fn [cs] (map #(-> %
                                   (update :x (partial + x))
                                   (update :y (partial + y)))
                              cs))))

(defn f [t]
  (let [r (* 200 (q/sin t) (q/cos t))]
    [(* r (q/sin (* t 0.2)))
     (* r (q/cos (* t 0.2)))]))

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
    (doseq [c @circles]
      (if (mouse-in-circle? c)
        (draw-highlight-circle c)
        (draw-normal-circle c))
      (q/fill 255))))

(defn setup []
  (init-circles)
  (q/frame-rate 60)
  (q/background 100))

(defn move-focus-to-mouse []
  (let [[x-diff y-diff] (map - [(q/mouse-x) (q/mouse-y)] [(:x (get-focused-circle)) (:y (get-focused-circle))])]
    (move-circles x-diff y-diff)))

(defn mouse-dragged []
  (let [x-diff (- (q/mouse-x) (q/pmouse-x))
        y-diff (- (q/mouse-y) (q/pmouse-y))]
    (move-circles x-diff y-diff)))

(q/defsketch trigonometry
  :size [600 600]
  :setup setup
  :draw draw
  :mouse-dragged mouse-dragged)
