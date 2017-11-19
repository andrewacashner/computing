(define add-float
  (lambda (alist label)
    (let ([pair (assoc label alist)])
      (if (eq? #f pair)
        [set! alist (acons label 0 alist)]
        [set-cdr! pair (+ 1 (cdr pair))]))))

(define figures '())

(set! figures (acons "gorilla" 0 figures))
(set! figures (acons "ape" 0 figures))



        


