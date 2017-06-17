;; Learning about scope, 2017/06/17
;; from tspl4

(define make-counter 
  (lambda ()
    (let ([next 0])
      (lambda ()
        (let ([v next])
          (set! next (+ next 1))
          v)))))

(define count1 (make-counter))
(define count2 (make-counter))

