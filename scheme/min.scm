;; min.scm -- find minimum value in list
;; Andrew Cashner, 2017/03/29
(define (minimum ls) 
  (letrec
    ((inner-min
      (lambda (ls current)
        (cond ((null? ls)
               current)
              ((< current (car ls)) 
               (inner-min (cdr ls) current))
              (else 
                (inner-min (cdr ls) (car ls)))))))
     (inner-min (cdr ls) (car ls))))

(define ls '(3 1 2 3 2 15))
(display (minimum ls))
(newline)
