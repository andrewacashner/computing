(+ 3 3)

(let ((x 3)) (+ x x))

((lambda (x) (+ x x)) 3)

(let ((double (lambda (x) (+ x x)))) (double 3))

(define double
  (lambda (x) (+ x x)))
(double 3)

(define (Double x)
  (+ x x))
(Double 3)

(let ((inc (lambda (x y) (+ x y)))) (inc 2 3))

(define inc
  (lambda (base current)
    (+ base current)))
(inc 2 3)

(define (Inc base current)
  (+ base current))
(Inc 2 3)
