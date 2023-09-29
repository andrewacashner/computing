(ns seesawFirst.core
  (:require [seesaw.core :as ss])
  (:gen-class))

(ss/native!)

(defrecord Event [date description])

(def Default-timeline (map map->Event 
                        [{:date "1981-04-11"
                          :description "Andrew born"}
                         {:date "1983-03-24"
                          :description "Ann born"}
                         {:date "2011-01-16"
                          :description "Ben born"}
                         {:date "2014-03-10"
                          :description "Joy born"}]))

(defn show-event
  [event]
  (:description event))

(defn sort-timeline
  [Timeline]
  (swap! Timeline update :timeline #(sort-by :date %)))

(defn create-view-children
  [Timeline]
  (let [instructions (ss/label :text "Click on the earliest event")

        timeline-box 
        (ss/listbox :model (map show-event (:timeline @Timeline))
                    :listen [:mouse-clicked
                             (fn [e] 
                               (if-let [choice (ss/selection e)]
                                (if (= choice 
                                       (show-event (:earliest @Timeline)))
                                 (ss/alert e "You got it!")
                                 (ss/alert e "Try again!"))))])
    
        sort-button 
        (ss/button :text "Sort"
                   :listen [:action (fn [e] (sort-timeline Timeline))])]

    (ss/vertical-panel :items [instructions timeline-box sort-button])))

(defn create-view
  [children]
  (ss/frame :title "Timeline"
            :visible? true
            :content children
            :on-close :exit
            :width 400
            :height 800))

(defn update-view
  [view Timeline]
  (fn [_ _ _ New-timeline]
      (ss/config! view :content (create-view-children Timeline))))

(defn state-logger
  [_ _ _ new-state]
  (println "State changed to: " new-state))

(defn -main
  [& args]
  (let [master-timeline (sort-by :date Default-timeline)
        Timeline (atom {:timeline (shuffle master-timeline)
                        :earliest (first master-timeline)})
        view (create-view (create-view-children Timeline))]
   
    (add-watch Timeline :update-view (update-view view Timeline))
    (add-watch Timeline :logger state-logger)
    
    (ss/invoke-now
      (-> view ss/pack! ss/show!))))

(comment
  ; re-implementation of increment adder (below) using master-view-controller
  ; model (after online tutorial)
(defn change-score
  [model change-fn]
  (swap! model update :score #(change-fn %)))

(defn reset-family 
  [model]
  (swap! model update :family #(shuffle %)))

(defn create-view-children
  [score family state score-change-fn family-change-fn]
  (let [score-label (ss/label :text "SCORE")
        score-value (ss/label :text score)

        score-box (ss/vertical-panel :items [score-label score-value])

        plus-button (ss/button :text "+1"
                               :listen [:action 
                                        (fn [e] (score-change-fn state inc))])
        minus-button (ss/button :text "-1"
                               :listen [:action 
                                        (fn [e] (score-change-fn state dec))])
        button-panel (ss/grid-panel :rows 1 :columns 2
                                    :items [plus-button minus-button])

        score-panel (ss/border-panel
                      :items [[score-box :north] [button-panel :south]])

        family-list (ss/listbox :model family)
        shuffle-button (ss/button :text "Shuffle"
                                  :listen [:action
                                           (fn [e] (family-change-fn state))])
        family-panel (ss/border-panel
                      :items [[family-list :north] [shuffle-button :south]])]
    
    (ss/border-panel
      :items [[family-panel :west] [score-panel :east]])))

(defn create-view
  [children]
  (ss/frame :title "Increment Calculator"
            :visible? true
            :content children
            :on-close :exit
            :width 400
            :height 300))

(defn update-view
  [view state]
  (fn [_ _ _ new-state]
    (let [score (:score new-state)
          family (:family new-state)]
      (ss/config! view :content (create-view-children score 
                                                      family
                                                      state 
                                                      change-score
                                                      reset-family)))))

(defn state-logger
  [_ _ _ new-state]
  (println "State changed to: " new-state))

(def Cashners ["Andrew" "Ann" "Ben" "Joy"])

(defn -main
  [& args]
  (let [init-score 0
        init-family Cashners
        init-winner (rand-nth init-family)
        state (atom {:score init-score
                     :family init-family
                     :winner init-winner})
        view (create-view (create-view-children init-score 
                                                init-family 
                                                state 
                                                change-score
                                                reset-family))]
    (add-watch state :update-view (update-view view state))
    (add-watch state :logger state-logger)
    (ss/invoke-now (fn [] (ss/show! (ss/pack! view))))))
)
(comment
  ; original mishmash timeline+increment adder mockup



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
  (ss/native!)
  (let [Names Cashners
        Answer (choose-answer Names)]
    (set-name-listener Answer)
    (set-shuffle-listener Names Answer)
    (ss/invoke-later
      (-> f
          ss/pack!
          ss/show!))))
)
