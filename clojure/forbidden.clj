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

(def lookup-first-pair (partial lookup-first-n 2))

(defn string-first       [s] (subs s 0 1))
(defn string-after-first [s] (subs s 1))
(defn string-after-pair  [s] (subs s 2))

(defn lookup-first
  [s]
  (or (lookup-first-n 1 s)
      (string-first s)))

(defn translate
  [s]
  (letfn [(do-translate 
            [orig trans]
            (if (empty? orig)
              trans
              (let [trans-two (lookup-first-pair orig)]
                (if trans-two
                  (do-translate (string-after-pair orig) 
                                (str trans trans-two))
                  (do-translate (string-after-first orig) 
                                (str trans (lookup-first orig)))))))]
    (do-translate s "")))

; TODO match case of original?

