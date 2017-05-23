;; Test run of autoscore.scm: Set up voice names and lyric specification
(load "autoscore.scm")

;; (define voices
;;   (list
;;     (voice+lyrics "SI") ; You could also write directly '("SI" #t . #f)
;;     (voice+lyrics "AI")
;;     (voice+lyrics "TI")
;;     (voice+figures "BI")))
 
(define voices
  (list
    (add-voice "SI" "TIPLE I"  "Ti. I" "vli")
    (add-voice "AI" "ALTUS I"  "A. I"  "vli")
    (add-voice "TI" "TENOR I"  "T. I"  "vl")
    (add-voice "BI" "BASSUS I" "B. I"  "vfi")))

(write-score "score.ly" voices)

