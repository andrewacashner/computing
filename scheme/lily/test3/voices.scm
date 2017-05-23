(load "../autoscore.scm")

(define voices
  (list
    (add-voice "Solo" "SOLO" "" "li")
    (add-voice "Bc"   "BASSO CONTINUO" "" "if")))

(define score
  (list
    (add-ungrouped-staves voices)))

(write-score "score.ly" score)
(write-input-outline "music.ly" voices)
