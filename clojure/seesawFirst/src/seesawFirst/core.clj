(ns seesawFirst.core
  (:require [seesaw.core :as ss]))

(ss/native!)

(def nameList (ss/listbox :model ["Andrew" "Ann" "Ben" "Joy"]))

(def Names ["Matthew" "Mark" "Luke" "John"])

(defn nameCard
  [name-str]
  (ss/text name-str))

(defn nameDeck
  [names]
  (ss/flow-panel :items (map nameCard names)))


(def addOne   (ss/button :text "+1"))
(def minusOne (ss/button :text "-1"))

(defn score-panel
  [n]
  (ss/vertical-panel :items [(format "Score: %d" n) addOne minusOne]))

(defn frame-contents 
  [n]
  (ss/border-panel 
    :west (score-panel n) 
    :center (nameDeck Names)
    :east nameList))

(def f (ss/frame :title "Game"
                 :content (frame-contents 0)
                 :on-close :exit))

(defn adjust-score 
  [this_atom this_frame func] 
  (ss/config! this_frame 
              :content (frame-contents (swap! this_atom func))))

(def Score (atom 0))

(defn inc-Score! [] (adjust-score Score f inc))
(defn dec-Score! [] (adjust-score Score f dec))

(ss/listen addOne   :action (fn [e] (inc-Score!)))
(ss/listen minusOne :action (fn [e] (dec-Score!)))

(ss/listen nameList :mouse-clicked
           (fn [e] (if-let [choice (ss/selection e)]
                     (when (= choice "Andrew") (inc-Score!)))))



(defn -main
  [& args]
  (ss/invoke-later
    (-> f
        ss/pack!
        ss/show!)))
