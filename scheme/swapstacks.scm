;; swapstacks.scm -- Andrew Cashner, 2017/10/26
;; Swap contents of two stacks; use extra stacks as needed

(define make-stack
  (lambda ()
    (let ([ls '()])
      (lambda (msg . args)
        (cond
          [(eq? msg 'empty?) (null? ls)]
          [(eq? msg 'top) 
           (if (null? ls) 
             '() 
             (car ls))]
          [(eq? msg 'push!)
           (set! ls (cons (car args) ls))]
          [(eq? msg 'pop!)
           (if (null? ls)
             "Empty stack" 
             (set! ls (cdr ls)))]
          [(eq? msg 'index)
           (list-ref ls (car args))]
          [(eq? msg 'length)
           (length ls)]
          [(eq? msg 'print)
           (begin 
             (display ls)
             (newline))]
          [else "Unknown message"])))))

(define list->stack!
  (lambda (stack ls)
    (if (not (null? ls))
      (begin
        (stack 'push! (car ls))
        (list->stack! stack (cdr ls))))))

(define swap!
  (lambda (s1 s2)
    "Push top of s1 onto s2, pop s1"
    (let ([top (s1 'top)])
      (if (not (null? top))
        (begin
          (s2 'push! top)
          (s1 'pop!))))))

(define dump-stack! 
  (lambda (s1 s2)
    "Pop all of stack1 and push onto stack2"
    (if (not (s1 'empty?))
      (begin
        (swap! s1 s2)
        (dump-stack! s1 s2)))))

(define swap-stacks!
  (lambda (s1 s2)
    (let ([s2-len (s2 'length)]
          [s3 (make-stack)]
          [s4 (make-stack)])
      (begin
        (dump-stack! s1 s3)
        (dump-stack! s2 s4)
        (dump-stack! s4 s1)
        (dump-stack! s3 s2)))))


(define stack->list!
  (lambda (stack)
    (if (stack 'empty?) 
      '()
      (let ([top (stack 'top)])
        (begin
        (stack 'pop!)
        (cons top (stack->list! stack)))))))

;; version using a list instead of a third stack
(define ls-swap-stacks!
  (lambda (s1 s2)
    (let* ([ls1 (reverse (stack->list! s1))]
           [ls2 (reverse (stack->list! s2))])
      (begin 
        (list->stack! s1 ls2) 
        (list->stack! s2 ls1)))))

;; this doesn't work
(define set-swap-stacks!
  (lambda (s1 s2)
    (let ([s3 (make-stack)])
      (begin 
        (set! s3 s1) 
        (set! s1 s2) 
        (set! s2 s3)))))

(define stackA (make-stack))
(define stackB (make-stack))

(list->stack! stackA '(0 1 2 3 4))
(list->stack! stackB '(a b c d e))

(stackA 'print)
(stackB 'print)

(swap-stacks! stackA stackB) 

(stackA 'print)
(stackB 'print)

