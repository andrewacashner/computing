(define cheers
  (lambda (n)
  (let inner-cheers ([count n] [ls '()])
    (if (<= count 1)
      (reverse (cons "Hurray" ls))
      (inner-cheers (- count 1) (cons "Hip" ls))))))

(define cheers-reverse
  (lambda (n)
  (let inner-cheers ([count n] [ls '("Hurray")])
    (if (<= count 1)
      (reverse ls)
      (inner-cheers (- count 1) (cons "Hip" ls))))))

(define cheers-ring
  (lambda (n)
  (let inner-cheers ([count n] [ls '()])
    (if (<= count 1)
      (append ls (cons "Hurray" ls))
      (inner-cheers (- count 1) (cons "Hip" ls))))))





