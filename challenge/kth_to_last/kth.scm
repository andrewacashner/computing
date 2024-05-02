; 2024/05/02

(define at-from-end
  (lambda (ls k)
    "Return the kth item from the end of a list"
    (list-ref (reverse ls) (- k 1))))
