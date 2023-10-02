; Graphical timeline game
; Andrew Cashner, 2023/10/02
;
; TODO 
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

(def make-clues shuffle)

(def isodate
  (tf/formatter "yyyy-MM-dd"))

(defn make-fact
  [m]
  (->Fact (tf/parse isodate (:date m)) 
         (:description m)))

(defn read-timeline [ms] (map make-fact ms))

(def fact-today
  "Starting condition of every timeline; just today's date"
  (->Fact (t/now) "Now"))

(def initial-timeline [fact-today])

(defn show-date
  "Return string with current date in YYYY-MM-DD format"
  [date]
  (tf/unparse isodate date))

(defn show-fact
  [fact]
  (format "%s: %s" (show-date (:date fact)) (:description fact)))

(def show-timeline (partial map show-fact))


(defn chrono-sort
  [timeline]
  (sort-by :date t/before? timeline))

(defn fact-before?
  [fact1 fact2]
  (t/before? (:date fact1) (:date fact2)))

(defn valid-answer?
  [clue guess timeline]
  (let [after (filter (partial fact-before? clue) timeline)]
    (= guess (first after))))

(defn index-of
  [coll item]
  (letfn [(match [index this-item] 
            (when (= item this-item) index))]
    (first (keep-indexed #(match %1 %2) coll))))

(defn find-matching-fact
  [search-term fact-strings fact-list]
  (let [pairs (map vector fact-strings fact-list)
        match (keep #(when (= (first %) search-term) (second %)) pairs)]
    (last match)))


(defn check-and-advance
  [state event]
  (if (empty? (:clues @state))
    (ss/alert event "No more clues")
    (if-let [guess (ss/selection event)]
      (let [old-timeline  (:timeline @state)
            old-display   (:timeline-display @state)
            test-fact     (find-matching-fact guess old-display old-timeline)
            old-clues     (:clues @state)
            new-fact      (first old-clues)
            new-clues     (rest old-clues)
            new-timeline  (chrono-sort (cons new-fact old-timeline))
            new-display   (show-timeline new-timeline)
            correct?      (valid-answer? new-fact test-fact new-timeline)
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

