#! /usr/bin/guile \
-e main -s
!#

#|
filename.scm
Andrew A. Cashner, 2020/03/09

USAGE: 
./filename.scm infile.txt outfile.tex

Takes as input a list of filenames in format like this:
    song: This I Believe
    text: Apostles' Creed
Prints output to file as list of LaTeX commands like this:
    \inputsong{this_i_believe}
    \inputtext{apostles_creed}
Where input is converted to appropriate command and standardized to filename.
|#

(use-modules
  (ice-9 format)
  (rnrs io ports))

(define string-split-remove-empty
  (lambda (str pred)
    "Do `(string-split str pred)' but remove empty elements in resulting list"
    (let* ([ls (string-split str pred)]
           [ls (delete "" ls)])
      ls)))

(define type->csname 
  (lambda (str)
    "Given string, check if it is is 'song' or 'text' and return TeX csname
    appropriate to each or give error"
    (let ([str (string-delete char-set:whitespace str)]) 
      (cond [(string= str "song") "inputsong"] 
            [(string= str "text") "inputtext"] 
            [else (error "Unknown input type (must be `song' or `text')")]))))

(define standardize-filename
  (lambda (str)
    "Remove punctuation chars from string, replace whitespace with underscore
    (treat consecutive whitespace as single)"
    (let* ([ls (string-split-remove-empty str char-set:whitespace)]
           [ls (map (lambda (s) (string-delete char-set:punctuation s)) ls)]
           [str (string-join ls "_")])
      str)))

(define read-songname
  (lambda (str)
    "Take string in form `song: Song Title' and return LaTeX command for
    including that song or text, selecting correct song/text type and
    standardizing filename derived from title"
    (let* ([str (string-downcase str)]
           [ls (string-split str #\:)]
           [csname (type->csname (car ls))]
           [basename (standardize-filename (cadr ls))]
           [cmd (format #f "\\~a{~a}" csname basename)])
      cmd)))

(define make-songlist
  (lambda (infile outfile)
    "Given an input file containing a list of strings in for `song: Song
    Title' or `text: Text Title', convert these to LaTeX input commands and
    write them to a file"
    (let* ([in-text (call-with-input-file infile get-string-all)]
           [lines (string-split-remove-empty in-text #\newline)]
           [commands (map read-songname lines)]
           [out-text (string-join commands "\n" 'suffix)])
      (call-with-output-file 
        outfile
        (lambda (port) (display out-text port))))))

(define main 
  (lambda (args)
    (let ([infile (list-ref args 1)]
          [outfile (list-ref args 2)])
      (make-songlist infile outfile))))

