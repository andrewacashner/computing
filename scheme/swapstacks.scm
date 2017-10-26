;; swapstacks.scm -- Andrew Cashner, 2017/10/26
;; Swap contents of two stacks; use extra stacks as needed

;; Pop all of stack A (contents a) and push onto empty stack C (now a is
;; reversed).
;; Pop all of stack B (contents b) and push onto empty stack D (now b is
;; reversed).
;; Pop all of stack C (contents a reversed) and push onto now-empty stack B, now
;; holds contents a in original order.
;; Pop all of stack D (contents b reversed) and push onto now-empty stack A, now
;; holds contents b in original order.

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
          [else "Unknown message"])))))

(define stackA (make-stack))
(define stackB (make-stack))
(define stackC (make-stack))
(define stackD (make-stack))

(stackA 'push! 0)
(stackA 'push! 1)
(stackA 'push! 2)
(stackA 'push! 3)
(stackA 'push! 4)

(stackB 'push! 'a)
(stackB 'push! 'b)
(stackB 'push! 'c)
(stackB 'push! 'd)
(stackB 'push! 'e)

(define dump-stack! 
  (lambda (s1 s2)
    "Pop all of stack1 and push onto stack2"
    (let dump ([s1-top (s1 'top)] 
               [s2 s2])
      (if (not (null? s1-top))
      (begin
        (s2 'push! s1-top) 
        (s1 'pop!)
        (dump (s1 'top) s2))))))

(define swap-stacks!
  (lambda (s1 s2 s3 s4)
    (begin
      (dump-stack! s1 s3)
      (dump-stack! s2 s4)
      (dump-stack! s3 s2)
      (dump-stack! s4 s1))))

