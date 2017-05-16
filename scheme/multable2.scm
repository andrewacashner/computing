;; multable.scm -- Andrew Cashner, 2017/05/15
;; print multiples of number #1 up to max #2

(define (inner-multable current base max) 
  "Make list of multiples"
  (if (> current max) 
    '() 
    (append (list current) (inner-multable (+ current base) base max))))

(define (multable num max) 
  "Print list of multiples"
  (begin (display (inner-multable num num max))
         (newline)))

(multable 3 21)
