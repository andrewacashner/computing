;; attempt at Scheme-only Lilypond input
;; Andrew Cashner, 2019/10/16

(include-subfiles '("music.scm" "lyrics.scm"))

(*TOP*
    (header (@ (title "Symphony no. 5 in C minor") 
               (composer "Ludwig van Beethoven") 
               (editor "Andrew A. Cashner")))
  (score
    (staff-group
      (staff (@ name "Flute") musicFlute)
      (staff (@ name "Clarinet") musicClarinet))
    (staff-group
      (staff (@ name "Trumpet") musicTrumpet)
      (staff (@ name "Horn") musicHorn))
    (staff-group
      (staff (@ name "Violin") musicViolin)
      (staff (@ name "Violoncello") musicVioloncello))))

      
     
