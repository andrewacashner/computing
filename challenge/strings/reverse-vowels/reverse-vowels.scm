; reverse the vowels in a string
; Andrew Cashner, 2024/07/05

(use-modules
  (srfi srfi-1))

(define vowels '(#\a #\e #\i #\o #\u))

(define indexed
  (lambda (ls)
    (letrec ([inner-index
               (lambda (in out count)
                 (if (null? in)
                   (reverse out)
                   (inner-index (cdr in)
                                (cons (cons count (car in)) out)
                                (+ 1 count))))])
               (inner-index ls '() 0))))

(define mark-vowels
  (lambda (str)
    (let inner-mark ([in-chars (string->list str)]
                     [partners (vowel-partners str)]
                     [out-chars '()])
      (if (null? in-chars)
        (reverse-list->string out-chars)
        (let ([this-char (car in-chars)]
              [this-matchup (car partners)])
        (if (eq? this-char (car this-matchup))
          (inner-mark (cdr in-chars) 
                      (cdr partners) 
                      (cons (cadr this-matchup) out-chars))
          (inner-mark (cdr in-chars)
                      partners
                      (cons this-char out-chars))))))))

(define vowel-partners
  (lambda (str)
    (let* ([chars (string->list str)]
           [matches (map (lambda (c) (if (member c vowels) c #f)) chars)]
           [hits (remove (lambda (m) (eq? #f m)) matches)]
           [partners (zip hits (reverse hits))])
      partners)))



