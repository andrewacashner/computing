(ns seesawFirst.core
  (:require [seesaw.core :as ss]))

(ss/native!)

(def Cashners ["Andrew" "Ann" "Ben" "Joy"])

(def nameList (ss/listbox :model Cashners))

(defn choose-answer
  [items]
  (rand-nth items))


(def button-plus-one   (ss/button :text "+1"))
(def button-minus-one  (ss/button :text "-1"))

(defn score-panel
  [n]
  (ss/vertical-panel 
    :items [(format "Score: %d" n) 
            button-plus-one 
            button-minus-one]))

(def button-shuffle (ss/button :text "Shuffle"))

(defn frame-contents 
  [n]
  (ss/border-panel 
    :west (ss/vertical-panel :items [nameList button-shuffle])
    :east (score-panel n)))

(def f (ss/frame :title "Game"
                 :content (frame-contents 0)
                 :on-close :exit))

(defn adjust-score 
  [this-atom this-frame func] 
  (ss/config! this-frame 
              :content (frame-contents (swap! this-atom func))))

(def Score (atom 0))

(defn inc-Score! [] (adjust-score Score f inc))
(defn dec-Score! [] (adjust-score Score f dec))

(ss/listen button-plus-one :action (fn [e] (inc-Score!)))

(ss/listen button-minus-one :action (fn [e] (dec-Score!)))

(defn set-name-listener
  [this-name]
  (ss/listen nameList :mouse-clicked
             (fn [e] (if-let [choice (ss/selection e)]
                     (if (= choice this-name) 
                       (inc-Score!)
                       (dec-Score!))))))

(defn reset-list-model
  [object ls]
  (ss/config! object :model ls))

(defn reset-namelist
  [names]
  (reset-list-model nameList names))

(defn set-shuffle-listener
  [names answer]
  (ss/listen button-shuffle :action 
             (fn [e] 
               (reset-namelist (shuffle names)))))

(defn -main
  [& args]
  (let [Names Cashners
        Answer (choose-answer Names)]
    (set-name-listener Answer)
    (set-shuffle-listener Names Answer)
    (ss/invoke-later
      (-> f
          ss/pack!
          ss/show!))))
