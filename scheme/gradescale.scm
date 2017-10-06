#!/usr/bin/env sh
exec guile -e main -s "$0" "$@"
!#

(use-modules (ice-9 format))

(define letter-points
  (list
    '("A"   . 93)
    '("A-"  . 90)
    '("B+"  . 87)
    '("B"   . 83)
    '("B-"  . 80)
    '("C+"  . 77)
    '("C"   . 73)
    '("C-"  . 70)
    '("D"   . 60)
    '("F"   . 50)))

(define map-cdr
  (lambda (fn alist)
    (if (null? alist)
      '()
      (let ([pair (car alist)]) 
        (cons 
          (cons (car pair) (fn (cdr pair))) 
          (map-cdr fn (cdr alist)))))))

(define gradescale
  (lambda (total percent-ls)
    (let ([points-from-total
            (lambda (total percent)
              (* total (/ percent 100.0)))]) 
      (map-cdr 
        (lambda (x) 
          (points-from-total total x)) 
      percent-ls))))

(define gradescale->str
  (lambda (alist)
    (let ([display-gradepair 
            (lambda (pair) 
              (format #f "~a\t~1,2f\n" 
                      (car pair) (cdr pair)))])
    (apply string-append 
           (map display-gradepair alist)))))

(define main
  (lambda (args)
    (let ([total (string->number (cadr args))]) 
      (display (gradescale->str
        (gradescale total letter-points))))))

