;; Test run of autoscore.scm: Set up voice names and lyric specification
(load "autoscore.scm")

(add-include-files 
  (list
    "header.ly"
    "villancico-geometry.ly" 
    "villancico-font.ly"))

(define chI 
  (list 
    (add-voice "SI" "TIPLE I"  "Ti. I" "li") 
    (add-voice "AI" "ALTUS I"  "A. I"  "li")))
(define chII 
  (list 
    (add-voice "SII" "TIPLE II"  "Ti. II" "l") 
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

;;; OR, DO IT THIS WAY IF YOU DON'T HAVE ANY STAFF GROUPS 
;; (define voices 
;;   (list 
;;     (add-voice "SI" "TIPLE I"  "Ti. I" "li") 
;;     (add-voice "AI" "ALTUS I"  "A. I"  "li")))
;; 
;; (define score
;;   (list
;;     (add-ungrouped-staves voices)))

(write-score "score.ly" score)

