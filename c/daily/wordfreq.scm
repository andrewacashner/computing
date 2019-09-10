#!/usr/bin/env sh
exec guile -e main -s "$0" "$@"
!#
; List number of chars in each word and number of words in input

(use-modules
  (ice-9 format)
  (rnrs io ports))

(define main
  (lambda (args)
    (let* ([text (get-string-all (current-input-port))]
           [text (string-delete #\newline text)]
           [words (string-split text char-set:whitespace)]
           [wordcount (length words)]
           [charcount (map string-length words)]
           [charcount (map number->string charcount)]
           [charcount-str (string-join charcount " ")])
      (format #t "~a\n~d\n" charcount-str wordcount))))


