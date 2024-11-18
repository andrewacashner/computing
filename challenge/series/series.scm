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

(define MAX-ITER 1000000)
(define PRECISION (/ 1 MAX-ITER))

;; TODO r6rs exception handling (guard?)
(define series-converge
  (lambda (fn)
    (let inner-series-converge ([sum 0] [iter 1])
      (let* ([next (fn iter)]
             [new-sum (+ next sum)]
             [diff (- new-sum sum)])
        (cond
          [(> iter MAX-ITER) (throw 'exception "Did not converge")]
          [(< (abs diff) PRECISION) new-sum]
          [else (inner-series-converge new-sum (+ 1 iter))])))))
