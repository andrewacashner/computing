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
             'empty
             (car ls))]
           [(eqv? msg 'push!)
            (set! ls (cons (car args) ls))]
          [(eqv? msg 'pop!)
           (if (null? ls)
             'empty
             (set! ls (cdr ls)))]
          [else "unknown command"])))))

(define stack-init
  (lambda (stack n)
    (if (> 1 n)
      0 
      (begin
        (stack 'push! n) 
        (stack-init stack (- n 1))))))

(define check-position
  (lambda (a b)
    (and (not (eqv? a 'empty))
         (or (eqv? b 'empty)
             (< a b)))))

(define pop-swap
  (lambda (stack-a stack-b)
    (let ([a (stack-a 'top)]
          [b (stack-b 'top)])
      (if (check-position a b) 
        (begin 
          (stack-b 'push! a) 
          (stack-a 'pop!))
        #f))))

(define stack0 (make-stack))
(define stack1 (make-stack))
(define stack2 (make-stack))

(stack-init stack0 5)

(define tower-status
  (lambda (stack0 stack1 stack2) 
    (display
      (list
      (stack0 'top)
      (stack1 'top)
      (stack2 'top)))
    (newline)))

(tower-status stack0 stack1 stack2)
(pop-swap stack0 stack1) (tower-status stack0 stack1 stack2)
(pop-swap stack0 stack2) (tower-status stack0 stack1 stack2)
(pop-swap stack1 stack2) (tower-status stack0 stack1 stack2)

(pop-swap stack0 stack1) (tower-status stack0 stack1 stack2)
(pop-swap stack2 stack0) (tower-status stack0 stack1 stack2)
(pop-swap stack2 stack1) (tower-status stack0 stack1 stack2)
(pop-swap stack0 stack1) (tower-status stack0 stack1 stack2)
(pop-swap stack0 stack2) (tower-status stack0 stack1 stack2)




