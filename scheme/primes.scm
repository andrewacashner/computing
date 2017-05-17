(use-modules (srfi srfi-1))
;; List primes up to given max using Sieve of Eratosthenes
(define (unmarked minm maxm increment ls) 
  "Create list of all integers from minm to maxm separated by increment"
  (if (> minm maxm) 
    ls 
    (unmarked (+ increment minm) maxm increment (append ls (list minm)))))
(define all (unmarked 2 100 1 '()))
(define twos (unmarked 2 100 2 '()))
(define next (lset-difference eq? all (cdr twos)))
(define primes (list (car next)))
(define next-multiples 
  (let ([test (car (lset-difference eq? next primes))]) 
    (unmarked test 100 test '())))
(set! next (lset-difference eq? next (cdr next-multiples)))
(set! primes (append primes (list (car (lset-difference eq? next primes)))))
(define fives (unmarked 5 100 5 '()))
(set! next (lset-difference eq? next (cdr fives)))
(set! primes (append primes (list (car (lset-difference eq? next primes)))))
(display primes)
(newline)

;; this can't be the best way to do this!
;; see multiple solutions on Rosetta Code



