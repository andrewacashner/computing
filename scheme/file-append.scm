;; file-append.scm -- Andrew A. Cashner, 2017/05/15
;;
;; Append text to output file (or create new file if none exists)
;; Arguments: #1 string, #2 name of output file

(define (file-append msg outfile) 
  (let ((outfileport (open-file outfile "a")))
  (display msg outfileport) 
  (newline outfileport)
  (close-output-port outfileport)))

