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

(define parse-ly-note
  (lambda (note) 
    (let* ([chars (string->list note)]
           [len (- 1 (length chars))]
           [step (char-upcase (car chars))]
           [duration (list-ref chars len)]
           [octave (list-ref chars (- 1 len))])
  ; ... )))
  ;; make these separate functions


(define note:ly->xml
  (lambda (lynote)
    (let*
      ([lychars     (string->list lynote)]
       [lystep      (char-upcase (car lychars))] 
       [lyoctave    (octave->duration (list-ref lychars 1))] 
       ; ignoring accidentals for now, assuming there is an octave sign
       [lyduration  (cadr lychars)]
       [lytype      (duration->type lyduration)])
      (xml note
        (xml pitch
          (xml step lystep)
          (xml octave lyoctave))
          (xml duration lyduration)
          (xml type lytype)))))
;; better to make a list/data structure for xml note?

(define xml
  (lambda element contents)
  (string-append 
    "<" element ">"
    contents
    "</" element ">"))
