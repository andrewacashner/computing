; true or false game
; Andrew Cashner 2023/09/29

(ns truefalse.core
  (:require [seesaw.core :as ss])
  (:gen-class))


(ss/native!)

(defrecord Fact [real? info])

(def facts
  (letfn [(make-facts [bool coll] (map (partial ->Fact bool) coll))]
    (let [fact-map
          {:true ["Neil Armstrong landed on the moon."
                  "Mexico briefly had an Austrian emperor, Maximilian."
                  "Alan Turing nearly qualified as an Olympic runner."
                  "Scientists think whales and wolves evolved from the same ancestor."
                  "There are more microorganisms living on your body than there are people on the earth."]
           :false ["President Kennedy was assassinated by the Illuminati."
                   "The alien visitor who crashed at Roswell met with the US president."
                   "William Shakespeare was actually a pseudonym of a commune of hyper-intelligent voles."
                   "Mysterious codes in the Hebrew Bible say that TikTok will become sentient and overthrow the Roman Catholic Church."]}
          truths (make-facts true (:true fact-map))
          fakes (make-facts false (:false fact-map))]
      (concat truths fakes))))

(defn true-fact? [fact] (:real? fact))

(def random-fact rand-nth)

(defn make-clues [facts] (shuffle facts))

(defn check-answer
  [answer guess]
  (or
    (and (= answer true) (= guess "t"))
    (and (= answer false) (= guess "f"))))

(defn clue-prompt
  [clue]
  (format "\nTrue or false?\n%s\n(Type t or f, or q to quit)" (:info clue)))

(defn end-prompt
  [score]
  (format "End of quiz! Final score: %d" score))

(defn cli-game
  []
  (letfn [(ask [clues score]
            (if (empty? clues)
              (println (end-prompt score))
              (let [clue (first clues)
                    answer (:real? clue)]
                (println (clue-prompt clue))
                (let [guess (read-line)]
                  (if (= guess "q") 
                    (ask [] score)
                    (let [result (check-answer answer guess)
                          msg (if result "Right" "Wrong")
                          score (if result (inc score) (dec score))]
                      (println (format "%s! Score: %d" msg score))
                      (recur (rest clues) score)))))))]
    (ask (make-clues facts) 0)))

(defn advance-state!
  [event state msg score-change-fn]
  (if (= 1 (count (:clues @state)))
    (ss/alert event 
              (format "No more clues! Final score: %d" 
                      (:score @state)))
    (let [new-data {:clues (rest (:clues @state))
                    :score (score-change-fn (:score @state))}]
      (ss/alert event msg)
      (reset! state new-data))))

(defn check-reset
  [event state answer guess]
  (if (= answer guess) ; both true or both false
    (advance-state! event state "Right!" inc)
    (advance-state! event state "Wrong!" dec)))


(defn create-view-children
  [state]
  (let [clue (first (:clues @state))
        answer (:real? clue)
        clue-box (ss/text :text (:info clue)
                          :multi-line? true
                          :wrap-lines? true)
        instructions (ss/label :text "True or false?")
        true-button (ss/button :text "True"
                               :listen [:action 
                                        (fn [e] (check-reset e state answer true))])
        false-button (ss/button :text "False"
                                :listen [:action 
                                         (fn [e] (check-reset e state answer false))])
        interface (ss/grid-panel :rows 1 :columns 2
                                 :items [true-button false-button])
        score-box (ss/text (format "Score: %d" (:score @state)))]
    (ss/vertical-panel :items [instructions clue-box score-box interface])))
        
(defn create-view
  [children]
  (ss/frame :title "History Fact vs. Fiction"
            :visible? true
            :on-close :exit
            :minimum-size [640 :by 480]
            :content children))

(defn update-view
  [view state]
    (fn [_ _ _ new-state]
      (ss/config! view :content (create-view-children state))))

(defn state-logger
  [_ _ _ new-state]
  (println "State changed to: " new-state))

(defn -main
  [& args]
  (let [init-clues (make-clues facts)
        init-score 0
        state (atom {:clues init-clues
                     :score init-score})
        view (create-view (create-view-children state))]

    (add-watch state :update-view (update-view view state))
;    (add-watch state :logger state-logger)

    (ss/invoke-now
      (-> view ss/pack! ss/show!))))
