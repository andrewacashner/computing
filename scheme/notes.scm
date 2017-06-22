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
    (string (char-upcase (car (string->list note))))))

(define ly-duration
  (lambda (note)
    "Given Lilypond note command, extract the concluding numeric portion of the
    string and convert to integer"
    (string->number
              (substring note (string-index note char-set:digit)))))

(define duration
  (lambda (note beats)
    "Given Lilypond note command and number of beats per measure, 
    return string with XML duration value" 
    (number->string (/ beats (ly-duration note)))))

(define type 
  (lambda (note)
    (let ([dur (ly-duration note)])
      (cond [(= dur 1) "whole"]
            [(= dur 2) "half"]
            [(= dur 4) "quarter"]
            [(= dur 8) "eighth"]
            [(= dur 16) "sixteenth"]
            [else ""]))))
; TODO surely there's a better way, e.g. table lookup

(define alter
  (lambda (note)
    "Given Lilypond note command string, return string with number of half
    steps to adjust pitch"
    (number->string 
      (cond
        [(string-contains note "eses") -2]
        [(string-contains note "es")   -1] 
        [(string-contains note "isis")  2]
        [(string-contains note "is")    1]
        [else 0]))))

(define octave
  (lambda (note)
    (let ([octave-increment
            (cond [(string-contains note "'") 
                   (string-count note #\')]
                  [(string-contains note ",")
                   (- 0 (string-count note #\,))]
                  [else 0])])
      (number->string (+ 4 octave-increment)))))

(define xml-tag
  (lambda (tag . contents)
    "Given a string with the name of an XML tag/element, and an optional list of
    strings with the contents to be put within that tag, return a string with
    the contents enclosed in XML start and end tags"
    (string-append 
      "<" tag ">"
      (apply string-append contents)
      "</" tag ">")))

; TODO add error checking, check for empty values
(define ly->xml:note
  (lambda (note beats)
    (xml-tag "note" 
         (xml-tag "pitch"
              (xml-tag "step" (step note)) 
                (xml-tag "alter" (alter note)) 
                (xml-tag "octave" (octave note)))
         (xml-tag "duration" (duration note beats))
         (xml-tag "type" (type note)))))


