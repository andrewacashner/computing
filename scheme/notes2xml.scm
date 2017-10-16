(define ly->xml
  (lambda (note)
    (let*
      ([notels (string (car (string->list note)))] 
       [pitch (car notels)]
       [octave 
         (let 
           ([inc 
              (cond 
                [(eqv? (cadr notels) #\') 
                 (length (cdr notels))] 
                [(eqv? (cadr notels) #\,)
                 (- 0 (length (cdr notels)))]
                [else 0])])
           (+ 3 inc))])
    (display (string-append
               "<note>\n<pitch>"
               pitch
               "</pitch>\n<octave>"
               (number->string octave)
               "</octave>"))
    (newline))))

