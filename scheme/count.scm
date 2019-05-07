(define map-count
  (lambda (fn ls)
    (let loop ([ls ls] [n 0] [new '()])
      (if (null? ls)
          (reverse new)
          (let ([node (cons (fn (car ls)) n)])
            (loop (cdr ls) (1+ n) (cons node new)))))))
