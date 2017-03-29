;; filter.scm -- from Holm, Sketchy Lisp
;; Andrew Cashner, 2017/03/29
(define (filter p a)
  (cond ((null? a)
         '())
        ((p (car a))
         (cons (car a)
               (filter p (cdr a))))
        (else
          (filter p (cdr a)))))

(define (complement p)
  (lambda (x) (not (p x))))

(define (remove p a)
  (filter (complement p) a))
