;; Test run of autoscore.scm: Set up voice names and lyric specification
(load "autoscore.scm")

(define voices
  (list
    (voice+lyrics "SI") ; You could also write directly '("SI" #t . #f)
    (voice+lyrics "AI")
    (voice+lyrics "TI")
    (voice+figures "BI")))

(new-voicelist "main")
(add-voice "Soprano"    "vli")
(add-voice "Alto"       "vl")
(add-voice "Bass"       "vi")


(write-score "score.ly" voices)

