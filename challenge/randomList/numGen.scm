#!/usr/bin/env sh
exec guile -e main -s "$0" "$@"
!#

(use-modules
  (ice-9 format))

(set! *random-state* (random-state-from-platform))

(define rand-list 
  (lambda (max) 
    (map (lambda n (random max)) (iota max))))

(define list-to-string
  (lambda (ls) 
    (let ([num-strings (map (lambda (n) (format #f "~d" n)) ls)])
      (string-join num-strings))))

(define main 
  (lambda (args)
    (let* ([max (if (> (length args) 1)
                 (string->number (list-ref args 1))
                 100)]
           [nums (list-to-string(rand-list max))])
      (display nums)
      (newline))))

