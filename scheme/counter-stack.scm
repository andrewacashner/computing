(define make-counter
  (lambda ()
    (let ([next 0])
      (lambda () 
        (let ([this next]) 
          (set! next (+ next 1)) 
          this)))))
(define count1 (make-counter))

(define make-stack
  (lambda ()
    (let ([ls '()]) 
      (lambda (msg . args) 
        (cond 
          [(eq? msg 'empty?) 
           (null? ls)] 
          [(eq? msg 'push!) 
           (set! ls (cons args ls))] 
          [(eq? msg 'top) 
           (if (null? ls) 
             '() 
             (car ls))] 
          [(eq? msg 'pop!) 
           (if (null? ls) 
             "Error: empty stack" 
             (set! ls (cdr ls)))]
          [(eq? 'display)
           (print-stack ls)]
          [else "Error: unknown message"])))))

(define print-stack
  (lambda (ls)
    (if (null? ls)
      (newline)
      (begin
        (display (car ls)) 
        (print-stack (cdr ls))))))

