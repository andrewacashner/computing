;; listlen.scm -- Andrew A. Cashner, 2017/05/17
;; Return the length of a list (number of list items)
(define (listlen ls)
  (letrec
    ((inner-listlen
       (lambda (len ls)
         (if (null? ls)
           len
           (inner-listlen (+ 1 len) (cdr ls))))
       ))
    (inner-listlen 0 ls)))

;; (define nums (list 1 2 3 4 5))
;; (listlen nums)
;; ===> 5
