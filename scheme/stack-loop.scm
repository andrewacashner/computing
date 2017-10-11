;; 2017/10/10
(define make-stack
  (lambda ()
    (let ([ls '()])
      (lambda (msg . args)
        (cond
          [(eq? msg 'empty?)
           (null? ls)]
          [(eq? msg 'top)
           (if (null? ls)
             "Empty stack"
             (car ls))]
          [(eq? msg 'pop!)
           (if (null? ls)
             "Empty stack" 
             (set! ls (cdr ls)))]
          [(eq? msg 'push!)
           (set! ls (cons args ls))]
          [else 
            "Unknown message"])))))

(define stack1 (make-stack))
(define stack2 (make-stack))
(define stack3 (make-stack))

(define stacks (list stack1 stack2 stack3))

(define data (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

(define fill-stacks
  (lambda (stack-ls data-ls)
    (letrec 
      ([inner-fill-stacks 
         (lambda (stack-ls i max data-ls) 
           (if (null? data-ls) 
             "Done" 
             (let ([stack (list-ref stack-ls i)] 
                   [data (car data-ls)])
               (stack 'push! data)
               (inner-fill-stacks 
                 stack-ls
                 (if (>= i max) 
                   0
                   (+ 1 i))
                 max
                 (cdr data-ls)))))])
      (inner-fill-stacks stack-ls 0 2 data-ls))))

(fill-stacks stacks data)


            



