(load "../autoscore.scm")

(define voices
  (list
    (add-voice "Solo" "SOLO" "" "li")
    (add-voice "Bc"   "BASSO CONTINUO" "" "if")))

(define score
  (list
    (add-ungrouped-staves voices)))

(make-score-file score)
(make-input-files voices)
