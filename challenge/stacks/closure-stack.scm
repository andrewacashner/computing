(define make-stack
  (lambda ()
    (let ([stack '()])
      (lambda (msg . arg)
        (cond
          [(eq? msg 'peek) (car stack)]
          [(eq? msg 'pop!)  ((let* ([top (car stack)])
                               (set! stack (cdr stack))
                               top))] ; TODO doesn't work
          [(eq? msg 'push!) (set! stack (cons arg stack))]
          [(eq? msg 'stack) stack]
          [else "Invalid message"])))))

;; ;; doesn't work
;; (define stack
;;   (lambda ()
;;     (let ([core '()])
;;       (list 
;;         (cons 'push (lambda (item) (set! core (cons item core))))
;;         (cons 'pop (lambda () (let ([top (car core)]) (set! core (cdr core))
;;                              top)))
;;         (cons 'stack core)))))
