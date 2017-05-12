;; Print a list of numbers from given min to given max
(define (println string)
  (display string)
  (newline))
(define (inner-countup current-num max-num current-list)
  (if (> current-num max-num)
    current-list
    (inner-countup 
      (+ 1 current-num) 
      max-num 
      (append current-list (list current-num))
    )))
(define (countup min max)
  (println (inner-countup min max '())))

;; (countup 1 10)
