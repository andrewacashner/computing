;; List primes up to given max using Sieve of Eratosthenes
(define (unmarked minimum maximum increment ls) 
  "Create list of all integers from minimum to maximum"
  (if (> minimum maximum) 
    ls 
    (unmarked (+ increment minimum) maximum increment (append ls (list minimum)))
    ))
(define (delete item ls)
  "Remove one item from list"
  (filter 
    (lambda (x)
      (not (equal? x item)))
    ls))

;;; start:
;; (define ls (unmarked 2 max 1 '()))
;;; filter for multable of each num


