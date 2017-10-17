;; ly2xml.scm -- First tries converting Lilypond notes to MusicXML format
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
;; Assuming complete, well-formed, correct Lilypond input
;; e.g., b4 c''4 cis,,16 eses'8
;; *************************************************************************

(define step
  (lambda (note)
    "Given a string with a single Lilypond note command,
    return an uppercase char for the pitch name"
    (string (char-upcase (string-ref note 0)))))

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
  '((1   . "whole")
    (2   . "half")
    (4   . "quarter")
    (8   . "eighth")
    (16  . "sixteenth")
    (32  . "thirty-second")
    (64  . "sixty-fourth")
    (128 . "one-hundred-twenty-eighth")))
;; TODO add longa, breve

;; TODO verify duration will be valid, or deal with #f output of assq
(define type 
  (lambda (note)
    "Given Lilypond note command string, return string for MusicXML
    representation of note duration, by looking up Lilypond duration in
    association list 'duration-strings'"
    (let ([dur (ly-duration note)])
      (assq-ref duration-strings dur))))

(define alter-amt 
  '(("s"    . -1)
    ("ses"  . -2)
    ("es"   . -1)
    ("eses" . -2)
    ("is"   .  1)
    ("sis"  .  2)
    ("isis" .  2)))

;; TODO deal with #f result of assoc-ref
(define alter
  (lambda (note)
    "Given Lilypond note command string, return string with number of half steps
    to adjust pitch; use the portion of the command between the first character
    (= pitch name) and the non-letter chars that follow, the octave signs or
    duration"
    (let ([accidental
            (substring note 1 
                       (string-skip note char-set:letter))])
             (if (= 0 (string-length accidental))
               ""
               (number->string (assoc-ref alter-amt accidental))))))

(define octave
  (lambda (note)
    "Given Lilypond note command string, return string with octave number;
    find the portion between the letters and the numbers, the count of symbols
    is the amount to be added to the base octave 4; or subtract if symbol is comma"
    (let* ([octave-str
            (substring note 
                       (string-skip note char-set:letter)
                       (string-index note char-set:digit))]
           [octave-add 
             (string-length octave-str)]
           [octave-increment 
             (if (and (> octave-add 0) 
                      (equal? #\, (string-ref octave-str 0)))
               (- 0 octave-add)
               octave-add)])
      (number->string (+ 4 octave-increment)))))

(define xml-tag
  (lambda (tag . contents)
    "Given a string with the name of an XML tag/element, and an optional list of
    strings with the contents to be put within that tag, return a string with
    the contents enclosed in XML start and end tags"
    (string-append 
      "<" tag ">"
      (apply string-append contents)
      "</" tag ">\n")))

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

(define ly->xml
  (lambda (ly-music beats)
    "Convert a string containing a Lilypond music expression, convert to a
    single Music XML string" 
    (let*
      ([start (+ 1 (string-index ly-music #\{))]
       [end (string-index ly-music #\})]
       [note-str
         (substring ly-music start end)]
       [note-ls
         (string-tokenize note-str)]
       [xml (apply string-append 
             (map 
               (lambda (note) (ly->xml:note note beats)) 
               note-ls))])
       (display xml))))
; TODO start from single Lilypond string of commands rather than list of strings
