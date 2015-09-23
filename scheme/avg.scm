(define length
  (lambda (ls)
    (if (null? ls)
	0
	(+ (length (cdr ls)) 1))))
(define sum
  (lambda (ls)
    (if (null? ls)
	0
	(+ (car ls) (sum (cdr ls))))))
(define avg
  (lambda (ls)
    (/ (sum ls) (length ls))))
	
