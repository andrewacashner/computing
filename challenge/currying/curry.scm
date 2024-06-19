(define partial
  (lambda (fn arg)
    (lambda (rest) (fn arg rest))))

(define plus-any
  (lambda (n)
    (partial + n)))

(define plus-two
  (lambda (n)
    ((partial + 2) n)))

(define plus-two-partial-a
  (lambda ()
    (lambda (n) ((plus-any 2) n))))

(define plus-two-partial-b
  (lambda (n)
    ((plus-any 2) n)))

(define plus-two-partial-c
  (lambda ()
    (plus-any 2)))

(define (plus-two-partial-d) (plus-any 2))

(define print
  (lambda (expr)
    (display expr)
    (newline)))

(print (identity 3))
(print (+ 1 2))
(print ((plus-any 1) 2))
(print (plus-two 1))
(print ((plus-two-partial-a) 1))
(print (plus-two-partial-b 1))
(print ((plus-two-partial-c) 1))
(print ((plus-two-partial-d) 1))
