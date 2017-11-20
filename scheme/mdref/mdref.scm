#!/usr/bin/env sh
exec guile -e main -s "$0" "$@"
!#

(use-modules
  (rnrs io ports)
  (ice-9 regex)
  (ice-9 eval-string))

(define member-index
  (lambda (obj ls)
    (let ([tail (member obj ls)])
      (if (eqv? #f tail)
        'not-found
        (- (length tail) 1)))))

(define ref-num
  (lambda (ls type label)
    (let ([label-ls (assoc-ref ls type)])
      (if (eqv? #f label-ls)
        'not-found
        (member-index label label-ls)))))

;; get the string at the end of the label list that matches type key,
;; find the number of the label in the list
(define ref
  (lambda (ls type label) 
    (let* ([label-str (car (last-pair (assoc-ref ls type)))]
           [label-num (ref-num ls type label)])
      (if (or (eqv? 'not-found label-str)
              (eqv? 'not-found label-num))
        "**??**"
        (string-append 
          label-str " " 
          (number->string label-num))))))

;; ls is alist where the values are lists ending with text for label strings,
;; add label to front of the list that matches the key
;; TODO insert string with float number
(define insert
  (lambda (ls type label)
    (let* ([label-ls (assoc-ref ls type)]
           [label-str (car (last-pair label-ls))]
           [index (length label-ls)])
      (begin 
        (assoc-set! ls type (cons label label-ls))
        (string-append 
          "[insert float: "
          label-str " "
          (number->string index)
          "]")))))
; side effect: sets ls values
 
(define process-file
  (lambda (infile outfile pattern data) ; data = float obj
    (let loop ([line (get-line infile)])
      (if (not (eof-object? line))
        (let ([output-str (process-line line pattern)])
          (begin
            (display output-str outfile)
            (loop (get-line infile))))))))
; side effect: display

(define process-line
  (lambda (line pattern)
    (let ([str (regexp-exec pattern line)])
      (if (eqv? #f str)
        (string-append line "\n")
        (string-append
          (match:prefix str)
          (eval-string (match:substring str 1))
          (match:suffix str)
          "\n")))))

(define match-insert 
  (make-regexp "`(\\(insert [^\\)`]*\\))`"))
(define match-ref    
  (make-regexp "`(\\(ref [^\\)`]*\\))`"))

(define float
  '((figure "Figure")
    (table  "Table")
    (music  "Music example")
    (poem   "Poem example")))

(define main
  (lambda (args)
    (let* ([infilename (cadr args)]
           [infile (open-file-input-port infilename)]
           [tmp-out (open-string-output-port)])
      (begin 
        (process-file infile tmp-out match-insert float) 
        (process-file 
          (open-string-input-port (get-output-string tmp-out)) 
          (current-output-port) match-ref float) 
        (close-port infile)))))
