#!/usr/bin/env sh
exec guile -e main -s "$0" "$@"
!#

(use-modules
  (srfi srfi-1)
  (ice-9 textual-ports))

(define main
  (lambda (args)
    (if (not (eq? (length args) 3))
      (format #t "Usage: reverse-lines INFILE OUTFILE\n")
      (let* ([infile-name (list-ref args 1)]
             [outfile-name (list-ref args 2)]
             [contents (reverse (lines infile-name))]
             [output (string-join contents "\n")])
        (call-with-output-file 
          outfile-name 
          (lambda (port) (display output port)))))))
 
(define lines 
  (lambda (infile-name)
    (let ([text (call-with-input-file infile-name get-string-all)])
      (string-split text #\newline))))

;; TODO trailing newline in input results in extra line in output

