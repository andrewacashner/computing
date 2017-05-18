;; insertstring.scm -- Andrew A. Cashner, 2017/05/17
(define (insertcsname csname) 
  "Insert given string into list and print result as single string"
  (begin (write (string-join 
             (list "\\do" csname "{" csname "}") "")) 
         (newline)))
