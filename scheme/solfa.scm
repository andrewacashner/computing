#!/usr/bin/env sh 
exec guile -e main -s "$0" "$@"
!#

(use-modules
  (ice-9 format))

(define scale '(ut re mi fa sol la))

(define gamut
  '((C sol fa ut)
    (D la sol re)
    (E #f la mi)
    (F ut #f fa)
    (G re ut sol)
    (A mi re la)
    (B-mol fa #f #f)
    (B-dur #f mi #f)))

(define member-index
  (lambda (key ls)
    (let ([tail (member key (reverse ls))])
      (and tail (length (cdr tail))))))

(define pitch
  (lambda (letter)
    (let* ([names   (assoc-ref gamut letter)]
           [mol     (list-ref names 0)]
           [dur     (list-ref names 1)]
           [nat     (list-ref names 2)]
           [i-mol   (member-index mol scale)]
           [i-dur   (member-index dur scale)]
           [i-nat   (member-index nat scale)]
           [first-i (max i-mol i-dur i-nat)]
           [first-n (cond
                      [(= first-i i-mol) mol]
                      [(= first-i i-dur) dur]
                      [(= first-i i-nat) nat])])
      (format #f "~a (~a)" letter first-n))))
      ;(format #f "~a (~a, ~a, ~a)" letter mol dur nat))))
;; this isn't the right order for every pitch 
;; account for #f
;; check for errors

(define main
  (lambda (args)
    (let* ([input (cadr args)]
           [key (string->symbol input)]
           [name (pitch key)])
    (begin
      (display name)
    (newline)))))
