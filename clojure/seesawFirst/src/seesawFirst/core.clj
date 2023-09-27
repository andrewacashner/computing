(ns seesawFirst.core
  (:require [seesaw.core :as ss]))

(ss/native!)

(def nameList (ss/listbox :model ["Andrew" "Ann" "Ben" "Joy"]))

(def addOne   (ss/button :text "+1"))
(def minusOne (ss/button :text "-1"))

(defn score-panel
  [n]
  (ss/vertical-panel :items [(format "Score: %d" n) addOne minusOne]))

(defn frame-contents 
  [n]
  (ss/border-panel 
    :west (score-panel n) 
    :east nameList))

(def f (ss/frame :title "Game"
                 :content (frame-contents 0)
                 :on-close :exit))

(defn adjust-score [n] (ss/config! f :content (frame-contents n)))

(def Score (atom 0))
(defn inc! [a] (swap! a inc))
(defn dec! [a] (swap! a dec))

(ss/listen addOne   :action (fn [e] (adjust-score (inc! Score))))
(ss/listen minusOne :action (fn [e] (adjust-score (dec! Score))))

(ss/listen nameList :selection 
        (fn [e] (when (= (ss/selection e) "Andrew")
                  (adjust-score (inc! Score)))))



(defn -main
  [& args]
  (ss/invoke-later
    (-> f
        ss/pack!
        ss/show!)))
