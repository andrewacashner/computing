;; down.scm -- count down to zero
;; Holm, Sketchy Lisp
;; 2017/03/23

(define (down x)
  (if (zero? x)
    0
    (down (- x 1))))
