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
  (if (< (count s) n)
    nil
    (dictionary (str/lower-case (subs s 0 n)))))

(def forbidden-pair (partial lookup-first-n 2))

(defn subs-flip
  [& args]
  (apply subs (reverse args)))

(def string-after-first      (partial subs-flip 1))
(def string-after-first-pair (partial subs-flip 2))

(defn string-first
  [s]
  (subs s 0 1))

(defn forbidden-letter
  [s]
  (let [trial (lookup-first-n 1 s)]
    (if trial trial (string-first s))))

(defn parse
  [input output]
  (if (empty? input)
    output
    (let [double-trans (forbidden-pair input)]
      (if double-trans
        (parse (string-after-first-pair input) 
               (str output double-trans))
        (parse (string-after-first input) 
               (str output (forbidden-letter input)))))))

(defn translate
  [s]
  (parse s ""))

; TODO match case of original?

; TODO rewrite parse using reduce or fold?

