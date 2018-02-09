#!/usr/bin/env sh
exec guile -e main -s "$0" "$@"
!#

(use-modules
  (ice-9 rdelim)
  (ice-9 regex)
  (ice-9 format))

(define match-lang-regexp "^# \\(.*\\)$")
(define match-key-regexp  "^[:alnum:]*:[:alnum:]*$")

(define process-file
  (lambda (infile lang-pattern key-pattern ls)
    (let loop ([line (read-line infile 'concat)])
      (if (not (eof-object? line))
        (let ([lang (regexp-exec lang-pattern line)])
          (if (not (eqv? #f lang)) 
              (set! ls (cons lang ls)) 
              (loop (read-line infile 'concat))))))))

(define main
  (lambda (args)
    (if (or
          (null? args)
          (not (= (length args) 3)))
      (format #t "Usage: langid <keyfile> <bibfile>\n")
      (let* ([keyfilename (cadr args)] 
             [bibfilename (caddr args)]
             [keyfile     (open-input-file keyfilename)]
             [bibfile     (open-input-file bibfilename)]
             [match-lang  (make-regexp match-lang-regexp)]
             [match-key   (make-regexp match-key-regexp)]
             [key-ls      (process-file keyfile match-lang match-key '())])
        (begin
          (display key-ls)
          (close-port keyfile)
          (close-port bibfile))))))


