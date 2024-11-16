;; Series (sum of finite sequence over a given range)
;; Andrew Cashner, 2024/11/16

(define range
  (lambda (low high)
    (let ([terms (+ 1 (- high low))])
      (iota terms low 1))))

(define sequence
  (lambda (fn low high)
    (map fn (range low high))))

(define series
  (lambda (fn low high)
    (apply + (sequence fn low high))))

;; (series (lambda (x) (expt 3 x)) 0 4) => 121
