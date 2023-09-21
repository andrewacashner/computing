; timeline
; Andrew A. Cashner
; 2023/09/21
;
; Game in which you put a timeline of events in chronological order

(ns timeline.core
  (:gen-class))

; 2023/09/20
(require '[clojure.string   :as str]
         '[clj-time.core    :as t]
         '[clj-time.format  :as tf]
         '[clj-yaml.core    :as yaml])
         

; CREATE A TIMELINE
; A timeline is a list of Events
(defrecord Event [date place description])

; CREATE A TIMELINE
(def isodate-now
  "Return string with current date in YYYY-MM-DD format"
  (tf/unparse (tf/formatter "yyyy-MM-dd") (t/now)))

(def new-timeline
  "Starting condition of every timeline; just today's date"
  (list (->Event isodate-now "Here" "Now")))

(defn shuffle-timeline
  "Shuffle a list of events"
  [events]
  (shuffle events))

; READ A TIMELINE

; We read the events from a YAML file
; Format:
; - {
;   date: "YYYY-MM-DD",
;   place: "Place",
;   description: "Description"
; }
; - {
; ....
; }
(defn read-events
  "Read input from a YAML file; return a list of Events"
  [filename]
  (let [input (yaml/parse-string (slurp filename))]
    (map map->Event input)))


; TEST EVENT CHRONOLOGY
(defn before?
  "Is the date of one event before or equal to the second?"
  [event1 event2]
  (<= (compare (:date event1) (:date event2)) 0))

(defn after?
  "Is the date of one event after the second?"
  [event1 event2]
  (> (compare (:date event1) (:date event2)) 0))

(defn earliest?
  "Is this event before all others in the list?"
  [timeline event]
  (before? event (first timeline)))

(defn latest?
  "Is this event after all others in the list?"
  [timeline event]
  (after? event (last timeline)))

(defn before-index?
  "Is this event before the event at a given index in the list?"
  [timeline event index]
  (before? event (nth timeline index)))

(defn after-index?
  "Is this event after the event at a given index in the list?"
  [timeline event index]
  (after? event (nth timeline index)))

(defn insert-at?
  "Is the date of this event between the events before the given index and at the given index?"
  [timeline event index]
  (and (before-index? timeline event index)
       (after-index? timeline event (dec index))))

; Main testing function
(defn timeline-insert-valid?
  "Is this a valid place to insert this event?"
  [timeline event index]
  (cond 
    (empty? timeline)           true
    (< index 0)                 false
    (= index 0)                 (earliest? timeline event)
    (>= index (count timeline)) (latest? timeline event)
    :else                       (insert-at? timeline event index)))

; INSERT AN EVENT
(defn list-insert-index
  "Insert an item into a list at a given index (return new list)"
  [items item index]
  (let [[before after] (split-at index items)]
    (concat before [item] after)))

; DISPLAY EVENTS
(defn event-string-clue
  "Format string for an event listed as a clue"
  [event]
  (format "(%s) %s" (:place event) (:description event)))

(defn event-string-indexed
  "Format string for an event as part of an ordered timeline"
  [index event]
  (format "%d. %s (%s) %s" (inc index) (:date event) (:place event) (:description event)))

(defn timeline-string
  "Return an ordered listing of a timeline as a string"
  [timeline]
  (if (empty? timeline)
    ""
    (str/join "\n" (map-indexed #(event-string-indexed %1 %2) timeline))))

; Messages to user
(defn status-turn-start
  "Message to user at start of turn, with current timeline and new event, prompt for input"
  [timeline clue]
  (str/join "\n"
           (list  "\n**************************"
                 "\nHere is the timeline you've made so far:"
                 (timeline-string timeline)
                 "\nNew event: When did this happen?"
                 (event-string-clue clue)
                 "\nWhich timeline number comes after the new event? [Enter a number:]")))

(defn status-game-over
  "Message at game end with total points, final timeline"
  [timeline points]
  (str/join "\n"
           (list  "\nGame over"
                 (format "Points: %d" points)
                 "\nFinal timeline:"
                  (timeline-string timeline))))

; CORE LOOP
(defn quiz
  "Core loop: Present items from randomly shuffled list of clues, prompt the user to add them to game timeline; if valid increment points and add new event in proper place, if not they lose a point and try again"
  [clues timeline points] 
  (if (empty? clues)
    (if (not (= timeline (sort-by :date  timeline)))
      (throw (Exception. "Error! Final timeline out of order!"))
      (println (status-game-over timeline points)))
    (let [clue (first clues)]
      (do
        (println (status-turn-start timeline clue))
        (let [index (dec (Integer/parseInt (read-line)))]
          (if (timeline-insert-valid? timeline clue index)
            (do
              (println (format "Correct! Points now: %d" (inc points)))
              (quiz (rest clues) 
                    (list-insert-index timeline clue index)
                    (inc points)))
            (do
              (println (format "Incorrect! Points now: %d" (dec points)))
              (quiz clues timeline (dec points)))))))))

(defn play
  "Set up timelines and play the game"
  [timeline]
  (quiz (shuffle-timeline timeline) new-timeline 0))

; MAIN FUNCTION
(def default-infile "src/timeline/input/default.yaml")

(defn -main
  "One optional argument is the name of a YAML file to read the timeline from; if not supplied use default-infile"
  [& args]
  (let [infile (if (empty? args) default-infile (first args))
        timeline (read-events infile)]
    (play timeline)))


