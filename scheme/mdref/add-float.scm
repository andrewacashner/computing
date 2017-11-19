(define member-index
  (lambda (obj ls)
    (let ([tail (member obj ls)])
      (if (eqv? #f tail)
        'not-found
        (- (length ls) 
           (length (member obj ls)))))))

(define ref-num
  (lambda (ls type label)
    (let ([label-ls (assoc-ref ls type)])
      (if (eqv? #f label-ls)
        'not-found
        (member-index label label-ls)))))

(define ref
  (lambda (ls type label) 
    (let ([label-str (car (assoc-ref ls type))]
          [label-num (number->string (ref-num ls type label))])
      (string-append label-str " " label-num))))

;;***************************

(define float
  '((figure "Figure" ape gorilla chimp orangutan)
    (music "Music example" Bach Mozart Gabrieli Hidalgo)
    (table "Table" parts wholes subparts)))

(ref float 'music 'Gabrieli)



