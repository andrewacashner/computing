(use-modules
  (srfi srfi-1))

(define sum-digits
  (lambda ()
    (let ([digits (iota 10)]
          [adder (lambda 
                   (n sum) 
                   (+ sum (* 5 (+ 9 (* 2 n)))))])
      (+ 1 (fold adder 0 digits)))))

(display (sum-digits))
(newline)
