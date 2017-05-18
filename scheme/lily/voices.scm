;; Test run of autoscore.scm: Set up voice names and lyric specification
(load "autoscore.scm")

(define voices
  (list
   (voice+lyrics "SI") ; You could also write directly '("SI" . #t)
   (voice+lyrics "AI")
   (voice+lyrics "TI")
   (voice "BI")))

(write-score "score.ly" voices)

