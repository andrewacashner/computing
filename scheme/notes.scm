;; first tries converting Lilypond notes to MusicXML format
;; AAC 2017/06/20
;;
;; (1) just the note part:
;;    `{ a'4 }`
;;     => `<note>
;;           <pitch>
;;             <step>A</step>
;;             <octave>4</octave>
;;           </pitch>
;;           <duration>1</duration>
;;           <type>quarter</type>
;;         </note>

(define step
  (lambda (note)
    "Given a string with a single Lilypond note command,
    return an uppercase char for the pitch name"
    (char-upcase (car (string->list note)))))

(define ly-duration
  (lambda (note)
    "Given Lilypond note command, extract the concluding numeric portion of the
    string and convert to integer"
    (string->number
              (substring note (string-index note char-set:digit)))))

(define duration
  (lambda (note beats)
    "Given Lilypond note command and number of beats per measure, 
    return integer for XML duration value" 
    (/ beats (ly-duration note))))

(define type 
  (lambda (note)
    (let ([dur (ly-duration note)])
      (cond [(= dur 1) "whole"]
            [(= dur 2) "half"]
            [(= dur 4) "quarter"]
            [(= dur 8) "eighth"]
            [(= dur 16) "sixteenth"]
            [else #f]))))
; TODO surely there's a better way, e.g. table lookup

(define alter
  (lambda (note)
    "Given Lilypond note command string, return integer with number of half
    steps to adjust pitch"
    (cond
      [(string-contains note "eses") -2]
      [(string-contains note "es")   -1] 
      [(string-contains note "isis")  2]
      [(string-contains note "is")    1]
      [else 0])))

(define octave
  (lambda (note)
    (let ([octave-increment
            (cond [(string-contains note "'") 
                   (string-count note #\')]
                  [(string-contains note ",")
                   (- 0 (string-count note #\,))]
                  [else 0])])
      (+ 4 octave-increment))))

(define note:ly->xml
  (lambda (lynote)
    (xml note 
         (((xml pitch
              (xml step (step lynote))
              (xml octave (octave lynote))
              (xml alter (alter lynote))) 
         (xml duration (duration lynote))
         (xml type (type (duration lynote))))))))
; TODO abbreviate with lets
; TODO add type

(define xml
  (lambda (element contents)
    (string-append 
      "<" element ">"
      contents
      "</" element ">")))
