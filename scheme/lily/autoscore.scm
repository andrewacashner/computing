;; automatically generate score using Scheme
(define (make-score voicelist text) 
  (if (null? voicelist) 
    text 
    (make-score 
      (cdr voicelist) 
      [string-append 
        text (string-append 
               (string-append 
                 "\\new Voice { \\Music" (car voicelist)) "}")])))
;; (define voices (list "SI" "AI" "TI" "BI"))
;; (display (make-score voices "")) (newline)
