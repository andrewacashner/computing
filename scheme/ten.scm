;; print numbers one to 10
;; 2018/01/02
(define nums 
  (lambda (n m) 
    (let loop ([n n]
               [ls '()])
      (if (> n m) 
        (begin 
          (display (reverse ls))
          (newline))
        (loop (+ 1 n) (cons n ls))))))
(define ten
  (lambda ()
    (nums 1 10)))

;; golfed version, 65 chars
(define f(let x([n 10][l '()])(if(< n 1)l(x(- n 1)(cons n l)))))f
;; or, same length
;; (let([f(lambda(n l)(if(< n 1)l(f(- n 1)(cons n l))))])(f 10 '()))
