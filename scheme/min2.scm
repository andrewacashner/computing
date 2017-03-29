;; min2.scm -- alternate attempt at finding minimum of list
;; Andrew Cashner, 2017/03/29
(define (minimum ls)
  (cond ((null? (cdr ls))
        (car ls))
        ((< (car ls) (minimum (cdr ls)))
         (car ls))
        (else 
          (minimum (cdr ls)))))

(define ls '(8 3 7 4 2 15))
(display (minimum ls))
(newline)
