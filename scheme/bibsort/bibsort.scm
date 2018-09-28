#!/usr/bin/env sh
exec guile -e main -s "$0" "$@"
!#

; bibsort.scm 
; Andrew Cashner, 2018/09/28

; Sort a bibtex file alphabetically by key

(use-modules 
  (srfi srfi-1)
  (rnrs io ports))

(define bibentry
  (lambda (s)
    "Select a single bibtex entry from a larger string, from one '@...{...}'
    to the next '@' or end; trim trailing newlines;
    return pair where car is the index of the end of this entry in the original
    string and cdr is the whole entry string"
    (let ([start (string-index s #\@)])
      (if (not start)
          #f
          (let* ([s-head (substring s start)]
                 [end (string-index s #\@ (+ 1 start))]
                 [end-pos (if end 
                              end 
                              (string-length s))]
                 [s-cut (if end 
                            (string-take s-head end) 
                            s-head)] 
                 [s (string-trim-right s-cut #\newline)])
            (cons end-pos s))))))

(define bibkey
  (lambda (s)
    "Select the key from a bibtex entry, from '@...{' to ','"
    (let ([start (string-index s #\{)])
      (if (not start)
          #f
          (let* ([start (+ 1 start)]
                 [end (string-index s #\, start)])
            (if (not end)
                #f
                (substring s start end)))))))

(define bibpair
  (lambda (s)
    "Find a bibtex entry in a larger string, create an object consisting of (1)
    index of the end of that entry in string, (2) pair where CAR is bibtex key
    and CDR is entire bibtex entry"
    (let* ([entry (bibentry s)]
           [value (cdr entry)]
           [end-pos (car entry)]
           [key (bibkey value)]) 
      (if (not key)
          #f
          (cons end-pos (cons key value))))))
            
(define bib->ls
  (lambda (s)
    "Make an association list of all the bibtex entries in a string;
    after one is found, skip ahead to the end of that one to scan for the next"
    (let loop ([s s] [ls '()])
      (if (string-null? s)
          (reverse ls)
          (let ([pair (bibpair s)])
            (if (not pair)
                (loop (string-drop s 1) ls)
                (let* ([end-pos (car pair)]
                       [entry (cdr pair)] 
                       [string-tail (string-drop s end-pos)])
                  (loop string-tail (cons entry ls)))))))))

(define key-cmp?
  (lambda (pair1 pair2)
    "Compare key strings of two bibtex entries"
    (string-ci<? (car pair1) (car pair2))))

(define sort-keys
  (lambda (ls)
    "Sort association list of bibtex entries by key"
    (sort-list ls key-cmp?)))

(define sort-bib
  (lambda (s)
    "Given string, locate bibtex entries and sort them alphabetically by key,
    return a string with the sorted bibliography"
    (let ([ls (bib->ls s)])
      (if (not ls)
          #f
          (let* ([ls-sort (sort-keys ls)] 
                 [bibstrings (fold-right 
                               (lambda (this acc) 
                                 (cons (cdr this) acc)) 
                               '() ls-sort)])
            (string-join bibstrings "\n\n" 'suffix))))))

(define quit-msg
  (lambda (s bool)
    "Quit with a message and an exit code; if bool is #t, use stdout and 0;
    if bool is #f, use stderr and EXIT_FAILURE"
    (let ([output (if bool 
                      (standard-output-port) 
                      (standard-error-port))])
    (begin
      (display s output)
      (quit bool)))))

(define main
  (lambda (args)
    "Read a bibtex file (arg 1), sort it, and write results to file 
    (optional arg 2) or standard output"
    (if (< (length args) 2)
        (quit-msg "- USAGE: bibsort <infile> [<outfile>]\n" #f)
        (let* ([infile (list-ref args 1)]
               [outfile (if (= 3 (length args))
                            (list-ref args 2)
                            'stdout)]
               [bib (call-with-input-file infile get-string-all)]
               [bib-sorted (sort-bib bib)])
          (if (not bib-sorted) 
              (quit-msg "- bibsort error: Unable to sort bibliography.\n" #f)
              (if (eq? outfile 'stdout)
                  (display bib-sorted) 
                  (call-with-output-file 
                    outfile 
                    (lambda (port) 
                      (display bib-sorted port)))))))))

