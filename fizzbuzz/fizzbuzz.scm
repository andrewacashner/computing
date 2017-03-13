;; Fizz Buzz, Andrew Cashner, 2017/03/12
;; Guile Scheme
(use-modules (ice-9 format))

(define (fzbz i max)
  (let ([output
    (cond ((= 0 (modulo i 15)) "FizzBuzz")
          ((= 0 (modulo i 3)) "Fizz")
          ((= 0 (modulo i 5)) "Buzz")
    (else i))])
  (format #t "~s " output))
  (if (>= i max)
    (newline)
    (fzbz (+ 1 i) max)))

(fzbz 1 100)
