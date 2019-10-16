; ms.scm
; Andrew Cashner, 2018/06/25
; experiments converting mensural notation

(use-modules
  (srfi srfi-1))

(define Meter 3)

(define tempus
  (lambda (s)
    (let ([meter 
            (assq-ref 
              '((C  . 2) 
                (CZ . 3)) 
              s)])
      (begin 
        (set! Meter meter)
        meter))))

(define name->minims
  (lambda (n group)
    (assq-ref 
      `((c  . 1/4) 
        (sm . 1/2)
        (m  . 1)
        (sb . ,group)
        (b  . ,(* 2 group)) 
        (l  . ,(* 4 group)))
      n)))

(define triple?
  (lambda (n)
    (if (> n 1) 
        (zero? (remainder n 3))
        #f)))

(define perfect
  (lambda (n)
    (if (triple? n)
        n
        (+ n (/ n 2)))))

(define imperfect
  (lambda (n)
    "If divisible by 3, subtract third of n; else return n"
    (if (triple? n)
        (- n (/ n 3))
        n)))

(define adjust-value
  (lambda (fn n group)
    (fn (name->minims n group))))

(define pt
  (lambda ls
    (map 
      (lambda (n) 
        (adjust-value perfect n Meter)) 
      ls)))

(define color
  (lambda ls
      (map 
      (lambda (n) 
        (list 'color (adjust-value imperfect n Meter)) )
      ls)))

(define ms
  (lambda (ls)
    (let* ([group 
            (eval (car ls) (interaction-environment))]
           [music 
             (fold-right 
               (lambda (this acc) 
                 (let ([value 
                         (if (list? this) 
                             (eval this (interaction-environment))
                             (list (imperfect (name->minims this group))))])
                   (append value acc)))
               '()
               (cdr ls))])
      (list (list 'meter group) music))))


; Al establo más dichoso
(define Al_establo 
  '((tempus 'CZ) 
    m sb 
    m sb
    (color 'm 'sb)
    sb m
    m sb
    (pt 'm) sm (color 'sb 'sb)
    sb m))


