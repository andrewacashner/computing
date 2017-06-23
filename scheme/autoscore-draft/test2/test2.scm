(load "../autoscore.scm")

(define voices
  (list
    (add-voice "S" "Soprano" "S." "li")
    (add-voice "A" "Alto" "A." "li")
    (add-voice "T" "Tenor" "T." "li")))

(define acc
  (list
    (add-voice "Bc" "Basso continuo" "Bc." "fi")))

(define score
  (list
    (add-staff-group "ChoirStaff" "ChI" "" voices)
    (add-ungrouped-staves acc)))

(make-score-file score)

(define outline
  (append voices acc))

; Write the outline of the music input file
(make-input-files outline)
