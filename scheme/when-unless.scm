;; tspl4 ex 3.1.4
;; syntax definition for when and unless
;; 2018-03-16

(define when '())
(define unless '())

(define-syntax when
  (syntax-rules ()
    [(_ test e1 e2 ...)
     (if test (begin e1 e2 ...))]))

(define-syntax unless
  (syntax-rules ()
    [(_ test e1 e2 ...)
     (when (not test) e1 e2 ...)]))
