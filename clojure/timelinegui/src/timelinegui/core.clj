; Graphical timeline game
; Andrew Cashner, 2023/10/02
;
; TODO 
; - better display all around
; - add images, audio/video to cards
; - better end-game action

(ns timelinegui.core
  (:require [seesaw.core      :as ss]
            [seesaw.chooser   :as sc]
            [clj-time.core    :as time]
            [clj-time.format  :as timef]
            [clj-yaml.core    :as yaml])
  (:gen-class))

(ss/native!)

(defrecord Fact [date description])

(def make-clues shuffle)

(def isodate
  (timef/formatter "yyyy-MM-dd"))

(defn make-fact
  [m]
  (->Fact (timef/parse isodate (:date m)) 
         (:description m)))

(defn read-timeline 
  [ms] 
  (map make-fact ms))

(defn read-timeline-file
  [filename]
  (let [input (yaml/parse-string (slurp filename))]
    (read-timeline input)))

(defn open-timeline-file 
  []
  (let [infile (sc/choose-file 
                 :all-files? false 
                 :filters [["YAML files" ["yaml"]]])]
    (read-timeline-file infile)))



(def fact-today
  "Starting condition of every timeline; just today's date"
  (->Fact (time/now) "Now"))

(def initial-timeline [fact-today])

(defn chrono-sort
  [timeline]
  (sort-by :date time/before? timeline))

(defn fact-before?
  [fact1 fact2]
  (time/before? (:date fact1) (:date fact2)))

(defn valid-answer?
  [clue guess timeline]
  (let [after (filter (partial fact-before? clue) timeline)]
    (= guess (first after))))

(def last-fact (map->Fact {:description "Game over"}))

(defn check-and-advance
  [state test-fact event]
  (if (= (first (:clues @state)) last-fact)
    (ss/alert event (format "Game over! Final score: %d" (:score @state)))
    (let [old-timeline  (:timeline @state)
          old-score     (:score @state)
          old-clues     (:clues @state)
          new-fact      (first old-clues)
          new-clues     (rest old-clues)
          new-timeline  (chrono-sort (cons new-fact old-timeline))
          correct?      (valid-answer? new-fact test-fact new-timeline)
          new-score     (if correct? (inc old-score) old-score)
          msg           (if correct? "Correct!" "Incorrect!")
          new-data      {:clues     (if (empty? new-clues) [last-fact] new-clues)
                         :timeline  new-timeline 
                         :score     new-score}]
      (ss/alert event msg)
      (reset! state new-data))))

(defn show-date
  "Return string with current date in YYYY-MM-DD format"
  [date]
  (timef/unparse isodate date))

(defn show-fact
  [fact]
  (format "%s\n\n%s" (show-date (:date fact)) (:description fact)))

(def card-options {:margin 10
                   :multi-line? true
                   :wrap-lines? true
                   :columns 10
                   :rows 10})

(defn clue-card
  [fact]
  (ss/text :text (:description fact)
           card-options))

(defn fact-card
  [state fact]
  (ss/text :text (show-fact fact)
           :listen [:mouse-clicked 
                    (partial check-and-advance state fact)]
           card-options))


(defn timeline-cards
  [state]
  (let [cards (map (partial fact-card state) (:timeline @state))]
    (ss/flow-panel :items cards)))


(defn create-view-children
  [state]
  (let [clue (first (:clues @state))

        instructions (ss/text :text 
                               "Click the item on the timeline that happened just AFTER the event in the clue."
                               :multi-line? true
                               :wrap-lines? true
                               :columns 20)
        clue-box (ss/flow-panel :items [(clue-card clue)])
        score-box (ss/text :text (format "Score: %d" (:score @state)))

        prompt-panel (ss/vertical-panel :items [instructions 
                                                clue-box
                                                score-box])

        timeline-box (timeline-cards state)]

    (ss/grid-panel :rows 2 :columns 1
                   :items [prompt-panel timeline-box])))


(defn create-view
  [children]
  (ss/frame :title "Timeline"
            :visible? true
            :on-close :exit
            :minimum-size [1920 :by 1080]
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
  (let [master    (if (empty? args)
                    (open-timeline-file)
                    (read-timeline-file (first args)))
        clues     (make-clues master)
        timeline  initial-timeline
        score     0
        state     (atom {:clues     clues
                         :timeline  timeline
                         :score     score})
        view (create-view (create-view-children state))]

    (add-watch state :update-view (update-view view state))
;    (add-watch state :logger state-logger)

    (ss/invoke-now (-> view ss/pack! ss/show!))))

