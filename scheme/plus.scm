(define (inner-plus current base max)
  (display current)
  (newline)
  (if (>= current max)
    0
    (inner-plus (+ base current) base max)))
(define (plus base max)
  (inner-plus base base max))
