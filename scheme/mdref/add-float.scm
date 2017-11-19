;; TODO
;; Read file and find sexps (insert float ...), set! float accordingly, replace
;; with nos
;; Read again and find sexps (ref float ...), evaluate and replace text

(define member-index
  (lambda (obj ls)
    (let ([tail (member obj ls)])
      (if (eqv? #f tail)
        'not-found
        (- (length tail) 1)))))

(define ref-num
  (lambda (ls type label)
    (let ([label-ls (assoc-ref ls type)])
      (if (eqv? #f label-ls)
        'not-found
        (member-index label label-ls)))))

;; get the string at the end of the label list that matches type key,
;; find the number of the label in the list
(define ref
  (lambda (ls type label) 
    (let* ([label-str (car (last-pair (assoc-ref ls type)))]
           [label-num (ref-num ls type label)])
      (if (or (eqv? 'not-found label-str)
              (eqv? 'not-found label-num))
        "**??**"
        (string-append label-str " " 
                       (number->string label-num))))))

;; ls is alist where the values are lists ending with text for label strings,
;; add label to front of the list that matches the key
;; TODO insert string with float number
(define insert
  (lambda (ls type label)
    (assoc-set! ls type (cons label (assoc-ref ls type)))))


(define float
  '((figure "Figure")
    (table  "Table")
    (music  "Music example")
    (poem   "Poem example")))
  
;;***************************

(insert float 'music 'Gabrieli)
(insert float 'music 'Mozart)
(ref float 'music 'Mozart)



