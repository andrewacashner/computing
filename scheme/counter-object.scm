;; 2017/10/10

(define counting
  (lambda ()
    (let ([count 0])
      (lambda (msg . args)
        (let ([n (if (null? args) 
                   1 
                   (car args))])
          (cond
            [(eq? msg 'inc)
             (set! count (+ count n)) 
             count]
            [(eq? msg 'dec)
             (set! count (- count n))
             count]
            [else 
              "Unknown message"]))))))

(define stepcounter
  (lambda (counter)
    (counter 'inc)))

(define addtocounter
  (lambda (counter n)
    (if (>= 0) 
      (counter 'inc n)
      (counter 'dec n))))

; (define count1 (counting))
; (count1 'inc)
; (count1 'inc 5)
; (count1 'dec 4)
; (stepcounter count1)
; (addtocounter count1 3)
; (addtocounter count1 -2)
