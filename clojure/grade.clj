; 2023/05/19
; Calculate a letter grade given points earned and total points possible

(def gradescale
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


(defn percent-of
  [amount total]
  (* 0.01 amount total))

(defn grade
  ([score total]
  (let [scale (into (sorted-map-by <) gradescale)
        match (last (take-while 
                      #(>= score (percent-of total (key %))) 
                      scale))]
    (val match)))
  ([score]
   (grade score 100)))


