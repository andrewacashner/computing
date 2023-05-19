; Working through intro documentation, https://clojure.org/reference
; 2023/05/19

(require '[clojure.string :as str])

; pre and post condition-maps
(defn inc-to-max 
  {:doc "Add 1 to given `n` up to `max-n`; throws assertion error if the number goes over"}
  [n max-n]
  {:post [(<= % max-n)]}
  (inc n))

(def grade-rubric 
  [["A+" 100.0] 
   ["A"   93.0]  
   ["A-"  90.0]  
   ["B+"  87.0]  
   ["B"   83.0]
   ["B-"  80.0]
   ["C+"  77.0]  
   ["C"   73.0]
   ["C-"  70.0]
   ["D+"  67.0]  
   ["D"   63.0]  
   ["D-"  60.0]  
   ["E"    0.0]])

;; Two approaches: 
;;  (1) use multiple map functions to transform all the scores and combine
;;  them with the letters; 
;;  (2) map a single function to transform each letter-score pair
(defn gradescale
  {:doc "Return a formatted table (string) with the minimum percentages equivalent to each letter-grade value out of a given total, according to a given grading scheme (map matching letter keys to numeric values out of 100)"}
  [scale total]
  (let [letters    (map first scale)
        old-points (map second scale)
        new-points (map #(* 0.01 total %) old-points)
        table-rows (map #(format "%s\t%4.1f" %1 %2) letters new-points)]
    (str/join "\n" table-rows)))

(defn gradescale2
  [scale total]
  (letfn [(row 
            [[grade score]] ; note destructuring (pattern matching)
            (let [new-points (* 0.01 total score)]
              (format "%s\t%4.1f" grade new-points)))]
    (str/join "\n" (map row scale))))

(defn grades [total] (gradescale grade-rubric total))

(defn print-grades [total] (println (grades total)))


