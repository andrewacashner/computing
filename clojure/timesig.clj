; Time signature
;
; Andrew Cashner, 2023/06/27
;
; Given a numeral for the number of beats, output a string
; with the Dorico time signature code for an additive 1/4 bar of that many
; beats, e.g., for input of 4 the output is [1+1+1+1]/4
(ns timesig
  (:require [clojure.string :as str]))

(defn sig
  [n]
  (let [beats (str/join "+" (repeat n "1"))]
    (str "[" beats "]/4")))

(let [args *command-line-args*]
  (if (= 1 (count args))
    (print (sig (read-string (first args))))
    (print "")))
