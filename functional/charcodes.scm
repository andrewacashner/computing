#!/usr/bin/guile \
-e main -s
!#
(use-modules (ice-9 rdelim))

(define charcodes
  (lambda (chars)
    (map char->integer chars)))

(define main
  (lambda (args)
    (let* ([infile (if (= 2 (length args))
                     (open-input-file (list-ref args 1))
                     (current-input-port))]
           [chars (string->list (read-line infile))]
           [codes (charcodes chars)])
      (write-line codes))))
