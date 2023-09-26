(ns seesawFirst.core
  (:require [seesaw.core :as ss]))

(ss/native!)

(def addOne   (ss/button :text "+1"))
(def minusOne (ss/button :text "-1"))

(defn panel
  [n]
  (ss/flow-panel :items [(format "Score: %d" n) addOne minusOne]))

(def f (ss/frame :title "Game"
                 :content (panel 0)
                 :on-close :exit))

(defn do!
  [func a]
  (reset! a (func @a)))

(def inc! (partial do! inc))
(def dec! (partial do! dec))

(def Score (atom 0))

(defn adjust-score [n] (ss/config! f :content (panel n)))
(defn inc-score [] (adjust-score (inc! Score)))
(defn dec-score [] (adjust-score (dec! Score)))

(ss/listen addOne   :action (fn [e] (inc-score)))
(ss/listen minusOne :action (fn [e] (dec-score)))


(defn -main
  [& args]
  (ss/invoke-later
    (-> f
        ss/pack!
        ss/show!)))
