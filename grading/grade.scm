    ; base letter = letter at index, index = points int/ 10 - 5
    ; quality = test of points % 10, 0 <= p < 3 : minus; 3 <= p < 7 : plain; 
   ; 7 <= p < p + 10 : +
(use-modules (srfi srfi-11))

(define points->letter
  (lambda (points)
    (cond [(= points 100) "A+"]
          [(< points 60) "E"]
          [else
            (let ([p (inexact->exact (round points))])
            (let-values ([(q r) (floor/ p 10)])
              (let* ([letters '("D" "C" "B" "A" "A+")]
                     [letter (list-ref letters (- q 6))] 
                     [letter (if (not letter ) "??" letter)]
                     [quality (cond [(and (<= 0 r) (< r 3)) "-"] 
                                    [(and (<= 7 r) (< r (+ 10 r))) "+"] 
                                    [else ""])])
                (string-append letter quality)))])))
