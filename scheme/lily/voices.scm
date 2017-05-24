;; Test run of autoscore.scm: Set up voice names and lyric specification
(load "autoscore.scm")

(add-include-files 
  (list
    "header.ly"))

(add-layout "indent = 2\\in")

(define chI 
  (list 
    (add-voice "SI" "TIPLE I"  "Ti. I" "li") 
    (add-voice "AI" "ALTUS I"  "A. I"  "li")))
(define chII 
  (list 
    (add-voice "SII" "TIPLE II"  "Ti. II" "li") 
    (add-voice "AII" "ALTUS II"  "A. II"  "l") 
    (add-voice "TII" "TENOR II"  "T. II"  "i") 
    (add-voice "BII" "BASSUS II" "B. II"  "fi")))
(define misc
  (list
    (add-voice "ThingI"  "THING I"  "Th. I"  "")
    (add-voice "ThingII" "THING II" "Th. II" "")))

(define score 
  (list
    (add-staff-group "ChoirStaff" "ChI"  "CHORUS I"  chI)
    (add-staff-group "ChoirStaff" "ChII" "CHORUS II" chII)
    (add-ungrouped-staves misc)))

(write-score "score.ly" score)


