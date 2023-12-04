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
            [seesaw.behave    :as sb]
            [seesaw.dnd       :as sd]
            [clj-time.core    :as time]
            [clj-time.format  :as timef]
            [clj-yaml.core    :as yaml])
  (:import [javax.swing SwingUtilities])
  (:gen-class))

(ss/native!)

(defrecord Fact [date description])

(def make-clues shuffle)

; We allow input in either yyyy or yyyy-mm-dd format, but we only display the
; year in the timeline. (Full dates allow more precise sorting.)
; TODO change or make configurable? 
(def isodate
  (timef/formatter time/utc "yyyy" "yyyy-MM-dd"))

(defn make-fact [m]
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

(defn show-date
  "Return string with the year of the given date"
  [date]
  (timef/unparse isodate date))

(defn show-fact
  [fact]
  (format "%s\n\n%s" (show-date (:date fact)) (:description fact)))


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


;from seesaw github seesaw/test/examples/xyz_panel.clj
(defn moveable
  [widget]
  (let [start-point (java.awt.Point.)]
    (sb/when-mouse-dragged 
      widget
      :start (fn [e] (ss/move! e :to-front)
               (.setLocation start-point (.getPoint e)))
      :drag (fn [e _]
              (let [p (.getPoint e)]
                (ss/move! e :by [(- (.x p) (.x start-point))
                                 (- (.y p) (.y start-point))])))
      :finish (fn [e] 
                (let [p (.getPoint e)]
                  (ss/alert e (format "Dragged to (%d, %d)" (.x p) (.y p)))
                  ; check if mouse release spot is inside correct box
                  ; or send click or other message to box at this spot
                  ; TODO use sd/default-transfer-handler from drag-and-drop
                  ; interface to transfer info from one object to another
                  )
                )))
      ; TODO move timeline cards to allow space for new card
      ; check if correct place
  widget)

(def center-align {:halign :center})

; TODO center card text or just center the year?
; (can't center a JTextArea (<- ss/text :multi-line? true)
(def card-options 
  {:margin 10
   :multi-line? true
   :wrap-lines? true
   :size [250 :by 350]})


(defn clue-card
  [fact]
  (moveable
  (ss/text :text (:description fact)
           card-options)))

(defn fact-card
  [state fact]
  (ss/text :text (show-fact fact)
           :listen [:mouse-clicked 
                    (partial check-and-advance state fact)
                    :mouse-moved
                    (fn [e] (ss/alert e "Dragged to here!"))]
           card-options))


(defn timeline-cards
  [state]
  (let [cards (map (partial fact-card state) (:timeline @state))]
    (ss/flow-panel :items cards)))


(def instruction-text 
  "Drag the event card to insert it into the timeline.")

(defn create-view-children
  [state]
  (let [clue           (first (:clues @state))
        instructions   (ss/text :text instruction-text center-align)
        clue-label     (ss/text :text "CLUE" center-align)
        clue-box       (clue-card clue)
        timeline-label (ss/text :text "TIMELINE" center-align)
        timeline       (timeline-cards state)
        score-box      (ss/text :text (format "Score: %d" (:score @state))
                                center-align)]

    (ss/vertical-panel :items [instructions 
                               clue-label
                               clue-box
                               timeline-label
                               timeline
                               score-box])))


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

