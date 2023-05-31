;try to rewrite with a "Point" object/record
; 2023/05/31

; Snake game, from "Programming in Clojure" (O'Reilly)
; Added limit that snake can't go outside window
(ns snake
  (:import (java.awt Color Dimension)
           (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event ActionListener KeyListener))
  (:require [clojure.set :refer :all]))

(load-file "import-static.clj")

;; Snake program start here
(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_UP VK_DOWN)

;; Setup
(def win-length 15)     ; was 5

(def width 60)          ; was 75
(def height 40)         ; was 50
(def point-size 10)     ; was 10
(def turn-millis 150)   ; was 75

(defrecord Point [x y])

(def dirs { VK_LEFT   (->Point -1  0)
            VK_RIGHT  (->Point 1  0)
            VK_UP     (->Point 0 -1)
            VK_DOWN   (->Point 0  1)})

(def light-green (Color.  15 160  70))
(def dark-green  (Color.   7  85  38))
(def red         (Color. 210  50  90))


(defn point-to-screen-rect 
  [pt]
  (map #(* point-size %)
       [(:x pt) (:y pt) 1 1]))

(defn add-points
  [{x1 :x, y1 :y} {x2 :x, y2 :y}]
  (->Point (+ x1 x2) (+ y1 y2)))

;(defn add-points
;  ([] 0)
;  ([x] x)
;  ([{x1 :x, y1 :y} {x2 :x, y2 :y}]
;   (->Point (+ x1 x2) (+ y1 y2)))
;  ([x y & more]
;   (reduce sum-points (sum-points x y) more)))

;(defn add-points 
;  [& pts]
;  (vec (apply map + pts)))

;; Apple and snake
(defn create-apple []
  {:location [->Point (rand-int width) (rand-int height)]
   :color red
   :type :apple})

(defn create-snake []
  {:body (list [(->Point 1 1)])
   :dir (->Point 1 0)
   :type :snake
   :color dark-green
   :head-color light-green})

;; Movement
(defn out-of-window?
  [{:keys [x y]}]
  (or (> x width)
      (> y height)
      (< x 0)
      (< y 0)))

(defn move 
  [{:keys [body dir] :as snake} & grow]
  (let [old-head  (first body)
        test-head (add-points old-head dir)
        tail      (if grow body (butlast body))
        new-body  (if (out-of-window? test-head)
                    body
                    (cons test-head tail))]
  (assoc snake :body new-body)))

(defn win? 
  [{body :body}]
  (>= (count body) win-length))

(defn head-overlaps-body? 
  [{[head & body] :body}]
  (contains? (set body) head))

(def lose? head-overlaps-body?)

(defn eats? 
  [{[snake-head] :body} {apple :location}]
  (= snake-head apple))

(defn turn
  [snake newdir]
  (assoc snake :dir newdir))

;; Mutable portion: reset, update
(defn reset-game 
  [snake apple]
  (dosync (ref-set apple (create-apple))
          (ref-set snake (create-snake)))
  nil)

(defn update-direction
  [snake newdir]
  (when newdir (dosync (alter snake turn newdir))))

(defn update-positions
  [snake apple]
  (dosync
          (if (eats? @snake @apple)
            (do (ref-set apple (create-apple))
                (alter snake move :grow))
            (alter snake move)))
  nil)

;; GUI
(defn fill-point
  [g pt color]
  (let [[x y width height] (point-to-screen-rect pt)]
    (.setColor g color)
    (.fillRect g x y width height)))

(defmulti paint 
  (fn [g object & _] (:type object)))

(defmethod paint :apple 
  [g {:keys [location color]}]
  (fill-point g location color))

(defmethod paint :snake 
  [g {:keys [body color head-color]}]
  (let [head (fill-point g (first body) head-color)
        tail (doseq [point (rest body)] 
               (fill-point g point color))]
    (cons head tail)))

(defn game-panel 
  [frame snake apple]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (paint g @snake)
      (paint g @apple))
    (actionPerformed [e]
      (update-positions snake apple)
      (when (lose? @snake)
        (reset-game snake apple)
        (JOptionPane/showMessageDialog 
          frame 
          "You ate your own tail! You lose!"))
      (when (win? @snake)
        (reset-game snake apple)
        (JOptionPane/showMessageDialog 
          frame 
          "You ate all the apples! You win!"))
      (.repaint this))
    (keyPressed [e]
      (update-direction snake (dirs (.getKeyCode e))))
    (getPreferredSize []
      (Dimension. (* (inc width) point-size)
                  (* (inc height) point-size)))
    (keyReleased [e])
    (keyTyped [e])))

(defn game []
  (let [snake (ref (create-snake))
        apple (ref (create-apple))
        frame (JFrame. "Snake")
        panel (game-panel frame snake apple)
        timer (Timer. turn-millis panel)]
    (doto panel
      (.setFocusable true)
      (.addKeyListener panel))
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true))
    (.start timer)
    [snake, apple, timer]))

