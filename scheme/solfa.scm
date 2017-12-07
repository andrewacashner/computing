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

(define solfa-hex
  (lambda (letter hex)
    (let ([index (cond 
                   [(eq? hex 'nat) 0]
                   [(eq? hex 'dur) 1]
                   [(eq? hex 'mol) 2])]
          [solfa-ls (assoc-ref gamut letter)])
      (list-ref solfa-ls index))))

