;; tspl4 ex 3.1.3, define-syntax for let*
;; solution on p. 90
;; 2018-03-16

(define let* '())

(define-syntax let*
  (syntax-rules ()
    [(_ () e1 e2 ...) 
     (let () e1 e2 ...)] ; handles last step of recursion
    [(_ ((x1 v1) (x2 v2) ...) e1 e2 ...)
     (let ((x1 v1)) 
       (let* ((x2 v2) ...) e1 e2 ...))]))

(define y 2)
(let* ([x 1] [y x]) (display y))
