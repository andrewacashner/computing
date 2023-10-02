; Graphical timeline game
; Andrew Cashner, 2023/10/02
;
; TODO 
; - use actual data structures from time module for dates, and use their
; comparison functions
; - prompt for user to load YAML input, read input
; - better end-of-game action
; - better display all around

(ns timelinegui.core
  (:require [seesaw.core      :as ss]
            [clj-time.core    :as t]
            [clj-time.format  :as tf])
  (:gen-class))

(ss/native!)

(defrecord Fact [date description])

(def Default-timeline 
  [{:date "0001-12-25"
    :description "Birth of Jesus?"}
   {:date "1981-04-11"
    :description "Birth of Andrew Cashner"}
   {:date "2001-09-11"
    :description "Terrorist attack on World Trade Center, New York, NY"}
   {:date "2021-01-06"
    :description "Pro-Trump Insurrection at US Capitol"}])

(def read-timeline (partial map map->Fact))

(def make-clues shuffle)

(defn show-fact
  [fact]
  (format "%s: %s" (:date fact) (:description fact)))

(def show-timeline (partial map show-fact))

(def isodate-now
  "Return string with current date in YYYY-MM-DD format"
  (tf/unparse (tf/formatter "yyyy-MM-dd") (t/now)))

(def fact-today
  "Starting condition of every timeline; just today's date"
  (->Fact isodate-now "Now"))

(def initial-timeline [fact-today])



; TODO replace with function from time library using real dates
(defn before?
  "Is the date of one fact before or equal to the second?"
  [fact1 fact2]
  (<= (compare (:date fact1) (:date fact2)) 0))

(defn after?
  "Is the date of one fact after the second?"
  [fact1 fact2]
  (> (compare (:date fact1) (:date fact2)) 0))

(defn between?
  [fact pre post]
  (and (before? fact post)
       (after? fact pre)))

(defn index-of
  [coll item]
  (letfn [(match [index this-item] 
            (when (= item this-item) index))]
    (first (keep-indexed #(match %1 %2) coll))))

(defn valid-answer?
  [clue post-guess timeline]
  (if-let [index (index-of timeline post-guess)]
    (cond (or (<= 1 (count timeline)) 
              (= 0 index)) 
          (before? clue post-guess)

          :else (let [pre-guess (nth timeline (dec index))]
                  (between? clue pre-guess post-guess)))))

; TODO surely there is a better way of getting the selected item
; as a record than by searching in the list of formatted strings of the
; listbox and then using that index to lookup the record
(defn check-and-advance
  [state event]
  (if (empty? (:clues @state))
    (ss/alert event "No more clues")
    (if-let [guess (ss/selection event)]
      (let [guess-index   (index-of (:timeline-display @state) guess)
            old-timeline  (:timeline @state)
            test-fact     (nth old-timeline guess-index)
            old-clues     (:clues @state)
            new-fact      (first old-clues)
            new-clues     (rest old-clues)
            new-timeline  (sort-by :date (cons new-fact old-timeline))
            new-display   (show-timeline new-timeline)
            correct?      (valid-answer? new-fact test-fact old-timeline)
            old-score     (:score @state)
            new-score     (if correct? (inc old-score) old-score)
            msg           (if correct? "Correct!" "Incorrect!")
            new-data      {:clues            new-clues 
                           :timeline         new-timeline 
                           :timeline-display new-display
                           :score            new-score}]
      (ss/alert event msg)
      (reset! state new-data)))))


(defn create-view-children
  [state]
  (let [clue (first (:clues @state))

        instructions (ss/text :text 
                               "Click the item on the timeline that happened just AFTER the event in the clue."
                               :multi-line? true
                               :wrap-lines? true)
        clue-box (ss/text :text (:description clue)
                          :multi-line? true
                          :wrap-lines? true)
        score-box (ss/text :text (format "Score: %d" (:score @state)))

        prompt-panel (ss/vertical-panel 
                       :items [instructions clue-box score-box])

        timeline-box (ss/listbox 
                       :model (:timeline-display @state)
                       :listen [:mouse-clicked
                                (fn [e] (check-and-advance state e))])]
                                                     
    (ss/grid-panel :rows 1 :columns 2
                   :items [prompt-panel timeline-box])))


(defn create-view
  [children]
  (ss/frame :title "Timeline"
            :visible? true
            :on-close :exit
            :minimum-size [1280 :by 1025]
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
  ; TODO prompt to open timeline file
  (let [master            (read-timeline Default-timeline)
        clues             (make-clues master)
        timeline          initial-timeline
        timeline-display  (show-timeline timeline)
        score 0
        state (atom {:clues             clues
                     :timeline          timeline
                     :timeline-display  (show-timeline timeline)
                     :score             score})
        view (create-view (create-view-children state))]

    (println @state)
    (add-watch state :update-view (update-view view state))
    (add-watch state :logger state-logger)

    (ss/invoke-now (-> view ss/pack! ss/show!))))

