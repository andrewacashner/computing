(ns forbidden
  (:require [clojure.string :as str]))

(def dictionary
  {; single letters
   "a" "Si",
   "b" "Wa",
   "c" "La",
   "d" "Ni",
   "e" "Lu",
   "f" "Mi",
   "g" "Yi",
   "h" "He",
   "i" "In",
   "j" "Ge",
   "k" "Ki",
   "l" "Li",
   "m" "Ma",
   "n" "Nu",
   "o" "Ot",
   "p" "Pe",
   "q" "Un",
   "r" "Ri",
   "s" "Swa",
   "t" "Ti",
   "u" "Uv",
   "v" "Vi",
   "w" "Wu",
   "x" "Ya" ,
   "y" "Pu" ,
   "z" "Zi" ,
   ; double letters
   "oo" "Sela",
   "ou" "Walu",
   "ow" "Sule",
   "oa" "Wavan",
   "ai" "Yeka",
   "ea" "Maya",
   "ee" "Aya",
   "th" "Selu",
   "ch" "Melu",
   "sh" "Yaku"})

(defn lookup-first-n
  [n s]
  (when (>= (count s) n)
    (dictionary (str/lower-case (subs s 0 n)))))

(def lookup-two (partial lookup-first-n 2))

(defn string-first      [s] (subs s 0 1))
(defn string-after-one  [s] (subs s 1))
(defn string-after-two  [s] (subs s 2))

(defn lookup-one
  [s]
  (or (lookup-first-n 1 s)
      (string-first s)))

(defn translate
  [s]
  (letfn [(do-translate 
            [in out]
            (if (empty? in)
              out
              (if-let [translate-two (lookup-two in)]
                (do-translate (string-after-two in) 
                              (str out translate-two))
                (do-translate (string-after-one in) 
                              (str out (lookup-one in))))))]
  (do-translate s "")))

; TODO match case of original?

