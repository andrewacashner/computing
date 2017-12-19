(use-modules
  (ice-9 format))

(define scale '(ut re mi fa sol la)

(define gamut '((C sol fa ut) 
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
    (let* ([hexachord   (filter identity (assoc-ref gamut letter))]
           [name-degree (map (lambda (x) member-index x scale) hexachord)]
           [sort-degree (sort name-degree >)]
           [solfa       (map (lambda (x) (list-ref scale x)) sort-degree)]
           [solfa-str   (map symbol->string solfa)]
           [solfa-fmt   (string-join solfa-str ", ")]
           [letter-output 
             (if (or (eq? letter 'B-dur) 
                     (eq? letter 'B-mol))
               "B"
               (symbol->string letter))])
      (format #f "~a (~a)" letter-output solfa-fmt))))

(define solfa-hex
  (lambda (letter hex)
    (let ([index (cond 
                   [(eq? hex 'nat) 0]
                   [(eq? hex 'dur) 1]
                   [(eq? hex 'mol) 2])]
          [solfa-ls (assoc-ref gamut letter)])
      (list-ref solfa-ls index))))

