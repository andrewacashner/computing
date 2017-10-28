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


(define push-list!
  (lambda (stack ls)
    (if (not (null? ls))
      (begin
        (stack 'push! (car ls))
        (push-list! stack (cdr ls))))))

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
  (lambda (s1 i s2)
    "Pop given number of items from stack1 and push onto stack2"
    (let dump ([n i]
               [s1-top (s1 'top)] 
               [s2 s2])
      (if (and
            (> n 0) 
            (not (null? s1-top)))
        (begin
          (s2 'push! s1-top) 
          (s1 'pop!)
          (dump (- n 1) (s1 'top) s2))))))

(define s3 (make-stack))
(define swap-stacks!
  (lambda (s1 s2)
    (let ([s2-len (s2 'length)])
      (begin
        (dump-stack! s1 s3)
        (dump-stack! s2 s3)
        (dump-stack-ref! s3 s2-len s1)
        (dump-stack! s3 s2)))))

(define stackA (make-stack))
(define stackB (make-stack))
(define numbers '(0 1 2 3 4))
(define letters '(a b c d e))
(push-list! stackA numbers)
(push-list! stackB letters)

(display "Initial state:") (newline)
(display "A:") (stackA 'print) (newline)
(display "B:") (stackB 'print) (newline)
;
; (swap-stacks! stackA stackB)
; (display "Final state:") (newline)
; (display "A:") (stackA 'print) (newline)
; (display "B:") (stackB 'print) (newline)
