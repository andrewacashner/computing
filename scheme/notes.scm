;; notes.scm -- First tries converting Lilypond notes to MusicXML format
;; Andrew A. Cashner, 2017/06/20-22
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
;; *************************************************************************

(define step
  (lambda (note)
    "Given a string with a single Lilypond note command,
    return an uppercase char for the pitch name"
    (string (char-upcase (string-ref note 0))))

(define ly-duration
  (lambda (note)
    "Given Lilypond note command, extract the concluding numeric portion of the
    string and convert to integer"
    (let ([index (string-index note char-set:digit)])
      (string->number (substring note index)))))

(define duration
  (lambda (note beats)
    "Given Lilypond note command and number of beats per measure, 
    return string with XML duration value" 
    (let ([dur (ly-duration note)])
      (number->string (/ beats dur)))))
   ;; TODO how to get breve, longa from duration functions

(define duration-strings
  (list
    '("longa" . "longa")
    '("breve" . "breve")
    '(1 . "whole")
    '(2 . "half")
    '(4 . "quarter")
    '(8 . "eighth")
    '(16 . "sixteenth")
    '(32 . "thirty-second")
    '(64 . "sixty-fourth")
    '(128 . "one-hundred-twenty-eighth")))

;; TODO verify duration will be valid, or deal with #f output of assq
(define type 
  (lambda (note)
    "Given Lilypond note command string, return string for MusicXML
    representation of note duration, by looking up Lilypond duration in
    association list 'duration-strings'"
    (let ([dur (ly-duration note)])
      (cdr (assv dur duration-strings)))))

(define alter-strings
  (list
    '("eses" . -2)
    '("es"   . -1)
    '("s"    . -1)
    '("isis" .  2)
    '("is"   .  1)))

; TODO redo below to use ass. list;
; make a substring of the portion between the first letter and either the octave
; or numeric portion (first non-letter); use assv to search alter-strings for
; this to get the alteration amount
(define alter
  (lambda (note)
    "Given Lilypond note command string, return string with number of half
    steps to adjust pitch"
    (let ([alter-amt 
      (cond
        [(string-contains note "eses") -2]
        [(string-contains note "es")   -1] 
        [(string-contains note "isis")  2]
        [(string-contains note "is")    1]
        [else 0])])
      (if (= 0 alter-amt)
        ""
        (number->string alter-amt)))))

(define octave
  (lambda (note)
    "Given Lilypond note command string, return string with octave number"
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

(define xml-tag-nonempty
  (lambda (tag contents)
    "Return a single XML tag string only if the contents are not empty" 
    (if (= 0 (string-length contents))
      ""
      (xml-tag tag contents))))

(define ly->xml:note
  (lambda (note beats)
    "Given a Lilypond note command and the beats per measure, return a string
    with a complete MusicXML <note> element; omit <alter> if no accidental, omit
    <type> if there is no string corresponding to the numeric value"
    (xml-tag "note" 
         (xml-tag "pitch"
              (xml-tag "step" (step note)) 
              (xml-tag-nonempty "alter" (alter note))
              (xml-tag "octave" (octave note)))
         (xml-tag "duration" (duration note beats))
         (xml-tag-nonempty "type" (type note))))) 


