; 2023/09/20
(require '[clojure.string :as str])

(defrecord Event [date place description])

(def NineEleven 
  (->Event "2001-09-11" 
         "New York, NY, US"
         "Terrorist attack on the World Trade Center"))

(def COVIDPandemic
  (->Event "2020-03-01"
         "Global"
         "First major surge of COVID-19 Pandemic"))

(def WWIArmistice
  (->Event "1918-11-11"
         "Global"
         "World War I ends with armistice"))

(def Timeline
  (list COVIDPandemic NineEleven WWIArmistice))

(defn timeline-random
  [events]
  (shuffle events))

(defn before?
  [event1 event2]
  (> 0 (compare (:date event1) (:date event2))))

(defn after?
  [event1 event2]
  (<= 0 (compare (:date event1) (:date event2))))

(defn earliest?
  [timeline event]
  (before? event (first timeline)))

(defn latest?
  [timeline event]
  (after? event (last timeline)))

(defn before-index?
  [timeline event index]
  (before? event (nth timeline index)))

(defn after-index?
  [timeline event index]
  (after? event (nth timeline index)))

(defn insert-at?
  [timeline event index]
  (and (before-index? timeline event index)
       (after-index? timeline event (dec index))))

(defn timeline-insert-valid?
  [timeline event index]
  (cond 
    (empty? timeline)           true
    (< index 0)                 false
    (= index 0)                 (earliest? timeline event)
    (>= index (count timeline)) (latest? timeline event)
    :else                       (insert-at? timeline event index)))

(defn list-insert-index
  [items item index]
  (let [[before after] (split-at index items)]
    (concat before [item] after)))

(defn event-string-clue
  [event]
  (format "(%s) %s" (:place event) (:description event)))

(defn event-string-indexed
  [index event]
  (format "%d. %s (%s) %s" (inc index) (:date event) (:place event) (:description event)))

(defn timeline-string
  [timeline]
  (if (empty? timeline)
    "1. Now"
    (str/join "\n" (map-indexed #(event-string-indexed %1 %2) timeline))))

(defn status-turn-start
  [timeline clue]
  (str/join "\n"
            (list "Current timeline:"
                  (timeline-string timeline)
                  "Next event:"
                  (event-string-clue clue)
                  "Insert new event before which timeline entry? [Enter a number]:")))

(defn status-game-over
  [timeline points]
  (str/join "\n"
            (list "Game over"
                  (format "Points: %d" points)
                  "Final timeline:"
                  (timeline-string timeline))))

(defn quiz
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
  [timeline]
  (quiz (timeline-random Timeline) '() 0))



