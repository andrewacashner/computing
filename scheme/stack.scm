(define make-stack
  (lambda ()
    (let ([ls '()])
      (lambda (msg . args)
        (cond [(eqv? msg 'empty?) (null? ls)]
              [(eqv? msg 'top)    
               (if (not (null? ls)) 
                 (car ls)
                 "empty")]
              [(eqv? msg 'push!)  (set! ls (cons (car args) ls))]
              [(eqv? msg 'pop!)   (set! ls (cdr ls))]
              [else "oops"])))))

