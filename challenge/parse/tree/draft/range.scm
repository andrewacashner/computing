(use-modules
  (srfi srfi-1))

(define range-values 
  (lambda (fn min max increment) 
    "Return a list of values of y = f(x) for a range of values of x"
    (map y (iota max min increment))))

(define domain-range-pairs
  (lambda (fn min max increment)
    "Return a list of (x . y) pairs for y = f(x) for a range of values of x"
    (zip (iota max min increment) 
         (range-values y min max increment))))

#| 
> (define y (lambda (x) (+ x 1)))
> (range-values y 0 5 1)
=> ((0 . 1) (1 . 2) (2 . 3) (3 . 4) (4 . 5))
|#
