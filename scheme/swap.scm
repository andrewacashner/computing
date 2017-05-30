;; swap.scm -- Andrew A. Cashner, 2017/05/30
;; Swap pairs of letters in a string

(define (swaptoks str pair)
  "Given a string, make into list of character tokens; 
  compare them to the first of a given pair;
  if matched, replace with second of pair; otherwise leave character unchanged.
  Make the altered token list into a string and return the new string."
  (letrec
    ([inner-swaptoks 
      (lambda (new-tokenls old-tokenls pair) 
        (if (null? old-tokenls)
          new-tokenls
          (inner-swaptoks
            (append new-tokenls 
                    (list
                      (if (eqv? (car old-tokenls) (car pair)) 
                      (cdr pair) 
                      (car old-tokenls))))
            (cdr old-tokenls)
            pair)))]) 
    (list->string (inner-swaptoks '() (string->list str) pair))))

          
