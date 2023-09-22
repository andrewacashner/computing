(ns forbidden
  (:require [clojure.string :as str]))

(def letterDict
  {"a" "Si"
   "b" "Wa"
   "c" "La"
   "d" "Ni"
   "e" "Lu"
   "f" "Mi"
   "g" "Yi"
   "h" "He"
   "i" "In"
   "j" "Ge"
   "k" "Ki"
   "l" "Li"
   "m" "Ma"
   "n" "Nu"
   "o" "Ot"
   "p" "Pe"
   "q" "Un"
   "r" "Ri"
   "s" "Swa"
   "t" "Ti"
   "u" "Uv"
   "v" "Vi"
   "w" "Wu"
   "x" "Ya" 
   "y" "Pu" 
   "z" "Zi" })

(def doubleLetterDict
  {"oo" "Sela"
   "ou" "Walu"
   "ow" "Sule"
   "oa" "Wavan"
   "ai" "Yeka"
   "ea" "Maya"
   "ee" "Aya"
   "th" "Selu"
   "ch" "Melu"
   "sh" "Yaku"})

(defn forbiddenLetter
  [c]
  (let [maybeTrans (letterDict (str/lower-case c))]
    (if maybeTrans maybeTrans c)))

(defn forbiddenPair
  [cs]
  (if (>= (count cs) 2)
    (doubleLetterDict (str/lower-case (apply str (take 2 cs))))))

(defn parse
  [input output]
  (if (empty? input)
    (apply str output)
    (let [doubleTrans (forbiddenPair input)]
      (if doubleTrans
        (parse (drop 2 input) (concat output (seq doubleTrans)))
        (parse (rest input) (concat output (list (forbiddenLetter (first input)))))))))
        
(defn translate
  [s]
  (parse s '()))

; TODO match case of original?

; TODO rewrite parse using reduce or fold?

