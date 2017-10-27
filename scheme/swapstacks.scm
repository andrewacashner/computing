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
          [else "Unknown message"])))))


(define stackA (make-stack))
(define stackB (make-stack))

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

(define dump-stack-ref! 
  (lambda (s1 n s2)
    "Pop given number of items from stack1 and push onto stack2"
    (let dump ([n n]
               [s1-top (s1 'top)] 
               [s2 s2])
      (if (and
            (> 0 n) 
            (not (null? s1-top)))
        (begin
          (s2 'push! s1-top) 
          (s1 'pop!)
          (dump (- 1 n) (s1 'top) s2))))))

(define swap-stacks!
  (lambda (s1 s2)
    (let ([s3 (make-stack)]
          [s2-len (s2 'length)])
      (begin
        (dump-stack! s1 s3)
        (dump-stack! s2 s3)
        (dump-stack-ref! s3 s2-len s2)
        (dump-stack! s3 s2)))))

