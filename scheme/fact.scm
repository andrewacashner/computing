;; factorial, with tail-call recursion
;; from Holm, Sketchy Lisp 
;; 2017/03/23

(define (fact n)
  (letrec
    ((fact2
         (lambda (n r)
         (cond ((zero? n) r)
               (#t (fact2 (- n 1) (* n r)))))))
     (fact2 n 1)))
