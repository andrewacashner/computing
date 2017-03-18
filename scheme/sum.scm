#!/usr/bin/env guile
!#
(define (sum n)
    (display (inner-sum n)) (newline))
(define (inner-sum nums)
  (if (null? nums)
    0
    (+ (car nums) (inner-sum (cdr nums)))))
(define nums '(1 2 3 4))
(sum nums)
