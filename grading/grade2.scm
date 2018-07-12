    ; base letter = letter at index, index = points int/ 10 - 5
    ; quality = test of points % 10, 0 <= p < 3 : minus; 3 <= p < 7 : plain; 
   ; 7 <= p < p + 10 : +
(use-modules (srfi srfi-11))

(define points->letter
  (lambda (points) 
    (let* ([letters '("E" "D" "C" "B" "A" "A+")] 
           [qualities '("" "+" "-")]
           [p (inexact->exact (round points))]
           [letter-index 
             (cond [(>= p 97) 5] 
                   [(< p 60) 0] 
                   [else (- (floor-quotient p 10) 6)])]
           [quality-index 
             (let ([r (floor-remainder p 10)]) 
               (cond [(and (<= 0 r) (< r 3)) 2] 
                     [(and (<= 7 r) (< r (+ 10 r))) 1] 
                     [else 0]))]
           [letter (list-ref letters letter-index)]
           [quality (list-ref qualities quality-index)])
      (string-append letter quality))))
