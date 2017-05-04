;; (use-modules (ice-9 format))
;; (define (multable base current max)
;;   (format #t "~d " current)
;;   (if (< max current)
;;     (newline)
;;     (multable base (+ base current) max)))

;; (define (inner-multable multiplier start max)
;;   (format #t "~d" start)
;;   (if (<= max multiplier)
;;     0
;;     (inner-multable (+ 1 multiplier) (* start (+ 1 multiplier)) max)))
;; (define (multable num max)
;;   (inner-multable 1 num max))
;; (multable 2 20)

(define (inner-multable multiplier base max)
  (if (<= max multiplier)
    (base)
    ((display (* multiplier base))
     (inner-multable (+ 1 multiplier) base max))))

(define (multable base max)
  (inner-multable 1 base max))

;; (multable 2 20)

