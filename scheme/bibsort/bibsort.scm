#!/usr/bin/env sh
exec guile -e main -s "$0" "$@"
!#

; bibsort.scm
; Andrew A. Cashner, 2018/09/26

(use-modules 
  (srfi srfi-1)
  (rnrs io ports))

(setlocale LC_ALL "")

(define sublist-test
  (lambda (ls start end)
    "Test input for sublist function"
    (and
      (list? ls)
      (not (eq? #f (memq start ls)))
      (or (not (eq? #f (memq end (memq start ls)))) 
          (and (eq? start end) 
               (not (eq? #f (memq end (cdr (memq start ls))))))))))

(define sublist
  (lambda (ls start end . mode)
    "bibsort function: sublist ls start end . mode
    INPUT: list LS, starting character, ending character, optional mode
             Mode can be 'trim-before, 'trim-after, or 'trim-both
    RETURN: New list containing contents of LS that start with START
             and end with END: include or exclude START or END based on mode"

    (if (not (sublist-test ls start end))
        #f
        (let* ([mode (if (null? mode) #f (car mode))]
               [from-start (memq start ls)]
               [after-start (cdr from-start)]
               [head (if (or (eq? mode 'trim-before) 
                             (eq? mode 'trim-both)) 
                         after-start 
                         from-start)]
               [tail (memq end after-start)]) 
          ; don't scan first character when looking for end character in
          ; case both are the same
          (if (eq? #f tail) ; end-char not found, e.g., EOF
              head
              (let* ([drop-length (if (or (eq? mode 'trim-after) 
                                          (eq? mode 'trim-both)) 
                                      (+ 1 (length tail))
                                      (length tail))]
                     [trim-delimiter (drop-right head drop-length)]
                     [trim-newline (if (eq? (last trim-delimiter) #\newline)
                                       (drop-right trim-delimiter 1)
                                       trim-delimiter)])
                trim-newline))))))

(define bibkey
  (lambda (ls)
    "Return the key from a single bibtex entry"
    (sublist ls #\{ #\, 'trim-both)))

(define bibentry
  (lambda (ls)
    "Return a single, entire bibtex entry from a larger string"
    (sublist ls #\@ #\@ 'trim-after)))

(define bibpair
  (lambda (ls)
    "Return a pair where CAR is the biblatex key for a single entry
    and the CDR is the whole entry"
    (let ([entry (bibentry ls)])
      (if entry
          (let ([key (bibkey entry)])
            (if key 
                (cons key entry) 

                #f))
          #f))))

(define bib->ls
  (lambda (s)
    "Given an input string, create an association list of the bibtex entries
    within it in format ((key1 . entry1) (key2 . entry2) ...)"
    (let loop ([chars (string->list s)] [ls '()])
      (if (null? chars)
          (reverse ls)
          (let ([pair (bibpair chars)])
            (if pair
                 (let* ([biblength (length (cdr pair))]
                        [rest (list-tail chars biblength)])
                   (loop rest (cons pair ls)))
                 (loop (cdr chars) ls)))))))
            

(define alist-key-strcmp?
  (lambda (ls1 ls2)
    "Compare two keys in association list, given keys as lists of chars and
    comparing them as strings"
    (let ([key1 (list->string (car ls1))]
          [key2 (list->string (car ls2))])
    (string-ci<? key1 key2))))

(define sort-keys
  (lambda (s)
    "Sort association list of bibkeys and bibentries by keys"
    (let ([pairs (bib->ls s)])
    (sort-list pairs alist-key-strcmp?))))

(define sort-bib
  (lambda (s)
    "Given an association list of bibtex keys and entries,
    sort them and return a string with the sorted list"
    (let loop ([ls (sort-keys s)] [strings '()])
      (if (null? ls)
          (let ([final-strings (map list->string (reverse strings))])
            (string-join final-strings "\n\n" 'suffix))
          (loop (cdr ls) (cons (cdar ls) strings))))))

(define main
  (lambda (args)
    "Read a bibtex file, sort it, and write the results to file or standard output"
    (if (and args (< (length args) 2))
        (format #t "USAGE: bibsort <infile> [<outfile>]\n")
        (let* ([infile (open-file-input-port (list-ref args 1))] 
               [outfile (if (= 2 (length args)) 
                            (current-output-port)
                            (open-file-output-port (list-ref args 2)))]
               [bib (get-string-all infile)]
               [bib-sorted (sort-bib bib)])
          (display bib-sorted outfile)))))

