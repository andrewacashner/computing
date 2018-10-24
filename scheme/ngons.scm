(define ngon-parts
  (lambda (n l)
    (let loop ([ls '(1)] [l l])
      (if (<= l 1)
          (reverse ls)
          (let* ([inc (- n 2)]
                 [curr (car ls)]
                 [next (+ curr inc)])
            (loop (cons next ls) (- l 1)))))))


(define ngon
  (lambda (n l)
    (let ([parts (ngon-parts n l)])
      (let loop ([ls '(1)] [parts (cdr parts)])
        (if (null? parts)
            (reverse ls)
            (let ([next (+ (car ls) (car parts))])
              (loop (cons next ls) (cdr parts))))))))

(define ngon-ref
  (lambda (n i)
    (car (reverse (ngon n i)))))

(define pent 
  (lambda (n) 
    (/ (- (* 3 n n) n) 2)))
