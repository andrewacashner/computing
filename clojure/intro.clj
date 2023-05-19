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
  { 100  "A+",
     93  "A" ,
     90  "A-",
     87  "B+",
     83  "B" ,
     80  "B-",
     77  "C+",
     73  "C" ,
     70  "C-",
     67  "D+",
     63  "D" ,
     60  "D-",
      0  "E" })

; loop, recur
(defn gradescale
  {:doc "Return a formatted table (string) with the minimum percentages equivalent to each letter-grade value out of a given total, according to a given grading scheme (map matching letter keys to numeric values out of 100)"}
  [scale total]
  (let [points       (map #(* 0.01 total %) (keys scale))
        newscale     (zipmap points (vals scale))
        sorted-scale (into (sorted-map-by >) newscale)
        table-rows   (for [[score grade] sorted-scale]
                         (str (format "%4.1f" score) "\t" grade))]
    (str/join "\n" table-rows)))

(defn grades [total] (gradescale grade-rubric total))
