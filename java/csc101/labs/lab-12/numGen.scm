#!/usr/bin/env sh
exec guile -e main -s "$0" "$@"
!#

(use-modules
  (ice-9 format))

(set! *random-state* (random-state-from-platform))

(define randList 
  (lambda (max) 
    (map (lambda n (random max)) (iota max))))

(define printList
  (lambda (ls) 
    (for-each (lambda (n) (format #t "~d " n)) ls)))

(define main 
  (lambda (args)
    (let ([max (string->number (list-ref args 1))])
      (printList (randList max)))))

