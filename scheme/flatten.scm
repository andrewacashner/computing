;; LIST UTILITIES
;; From rosettacode.org
(define (flatten ls) 
  "Flatten list: ((one two) three (four)) => (one two three four)"
  (cond ((null? ls) '())
        ((not (pair? ls)) (list ls))
        (else (append (flatten (car ls))
                      (flatten (cdr ls))))))


