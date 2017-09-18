;; towers puzzle

(define make-stack
  (lambda ()
    (let ([ls '()])
      (lambda (msg . args)
        (cond
          [(eqv? msg 'empty?) 
           (null? ls)]
          [(eqv? msg 'top)
           (if (null? ls)
             "empty list" 
             (car ls))]
           [(eqv? msg 'push!)
            (set! ls (cons (car args) ls))]
          [(eqv? msg 'pop!)
           (if (null? ls)
             "empty list"
             (set! ls (cdr ls)))]
          [else "unknown command"])))))

(define stack0 (make-stack))
(define stack1 (make-stack))
(define stack2 (make-stack))


