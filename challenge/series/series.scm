#!/usr/bin/env sh
exec guile -e main -s "$0" "$@"
!#

;; Series (sum of finite sequence over a given range)
;; Andrew Cashner, 2024/11/16

(use-modules
  (ice-9 format))

(define range
  (lambda (low high)
    (let ([terms (+ 1 (- high low))])
      (iota terms low 1))))

(define sequence
  (lambda (fn low high)
    (map fn (range low high))))

(define series
  (lambda (fn low high)
    (apply + (sequence fn low high))))

;; (series (lambda (x) (expt 3 x)) 0 4) => 121

(define MAX-ITER 10000)
(define PRECISION 0.00001)

;; TODO r6rs exception handling (guard?)
(define series-converge
  (lambda (fn low)
    (let inner-series-converge ([sum low] [iter 1])
      (let* ([next (fn iter)]
             [new-sum (+ next sum)]
             [diff (- new-sum sum)])
        (cond
          [(> iter MAX-ITER) #f] ;; (throw 'exception "Did not converge")]
          [(< (abs diff) PRECISION) new-sum]
          [else (inner-series-converge new-sum (+ 1 iter))])))))

(define converges?
  (lambda (fn low)
    (if (not (series-converge fn low))
      #f
      #t)))

(define main
  (lambda (args)
    (if (not (eq? 3 (length args)))
      (format #t "Usage: series MIN MAX\n")
      (let ([low (string->number (list-ref args 1))]
            [high (string->number (list-ref args 2))]
            [fn (lambda (n) (expt 0.25 n))])
        (format #t "~f\n" (series fn low high))))))
