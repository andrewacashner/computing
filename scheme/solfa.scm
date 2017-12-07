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

(define solfa->degree
  (lambda (solfa)
    (member-index solfa scale)))

(define degree->solfa
  (lambda (degree)
    (list-ref scale degree)))

(define pitch
  (lambda (letter)
    (let* ([hexachords   (assoc-ref gamut letter)]
           [hex-true     (filter identity hexachords)]
           [name-degrees (map solfa->degree hex-true)]
           [sort-degrees (sort name-degrees >)]
           [solfa-ls     (map degree->solfa sort-degrees)]
           [solfa-ls-str (map symbol->string solfa-ls)]
           [solfa-output (string-join solfa-ls-str ", ")]
           [letter-output 
             (if (or (eq? letter 'B-dur) 
                     (eq? letter 'B-mol))
               "B"
               (symbol->string letter))])
      (format #f "~a (~a)" letter-output solfa-output))))

(define solfa-hexachord
  (lambda (letter hexachord)
    (let ([index 
            (cond 
              [(if (eq? hexachord 'nat) 0)]
              [(if (eq? hexachord 'dur) 1)]
              [(if (eq? hexachord 'mol) 2)])]) ; doesn't work
      (list-ref (assoc-ref gamut letter) index))))

(define main
  (lambda (args)
    (let* ([input (cadr args)]
           [key (string->symbol input)]
           [name (pitch key)])
    (begin
      (display name)
    (newline)))))
