(ns fill-prob.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  (q/frame-rate 1)
  (q/background 255)
  ;; Uncoment here if you want to render image,
  ;; the path should be under the data...
  ;(q/set-state! :image (q/load-image PATH))
  ;(q/resize-sketch (q/width) (* (q/width) (.-height (q/state :image))
  ;                           (/ 1 (.-width (q/state :image)))))
  (q/color-mode :rgb 255 255 255 1.))

(defn draw-dot
  [[x y] radius alpha]
  (let [inner (* radius 0.3)
        outer (- radius inner)
        step 0.01]
    (q/no-stroke)
    (q/fill 0 (* step alpha 10))
    (loop [r 0]
      (when (<= r 1)
        (let [cr (* 2 (+ inner (* (q/pow r 2) outer)))]
          (q/ellipse x y cr cr))
        (recur (+ r step))))))

(defn custom-distribution [pdf dimension]
  #(loop []
     (let [ts (repeatedly dimension rand)
           v (apply pdf ts)]
       (if (< (rand) v)
         (flatten [v ts])
         (recur)))))

(defn impulse [x]
  (let [h (* 10 x)]
    (* h (q/exp (- 0.3 h)))))


(def impulse-distribute (custom-distribution impulse 1))


(defn draw-impulse []
  (dotimes [i 1000]
    (let [[p x] (impulse-distribute)
          x (* (q/width) x)
          y (rand-h)
          radius (* p (/ (q/height) 15))]
      (draw-dot [x y] radius 0.12))))

;;;;;;;;;;;;;;;;;
;; 2D FUNCTION ;;
;;;;;;;;;;;;;;;;;

(defn mcos [x y]
  (let [[x y] (map #(* 8.0 (- % 0.5)) [x y])] ;; scale the coordinate
    (q/cos (* x y x))))

(def mcos-distribute (custom-distribution mcos 2))

(defn draw-mcos []
  (dotimes [i 1000]
    (let [[p x y] (mcos-distribute)
          x (* (q/width) x)
          y (* (q/height) y)
          radius (* p (/ (q/height) 80))]
      (draw-dot [x y] radius 0.10))))


;;;;;;;;;;;
;; IMAGE ;;
;;;;;;;;;;;


(defn image-pixel [x y]
  (let [image (q/state :image)
        w (.-width image)
        h (.-height image)
        [ix iy] (map q/floor [(* w x) (* h y)])
        pixel (q/get-pixel image ix iy)
        p (q/green pixel)]
    (q/map-range p 0 255 1 0)))

(def image-distribute (custom-distribution image-xy 2))

(defn draw-image []
  (dotimes [i 1000]
    (let [[p x y] (image-distribute)
          p (q/pow p 6)
          x (* (q/width) x)
          y (* (q/height) y)
          radius (* p (/ (q/height) 300))]      
      (draw-dot [x y] radius 0.12))))

(defn main []
  (q/defsketch Fill-The-Prob
    :title "Fill The Probablity"
    :features [:keep-on-top]
    :setup setup
    :draw draw-impulse
    :size [1000 618]
    :middleware [m/pause-on-error]
    :settings #(q/pixel-density (q/display-density))))

(main)
