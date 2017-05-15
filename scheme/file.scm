(define (write-log file)
  (let ((msg 
          (lambda (file)
            (display "Successful call to write-log" file)
            (newline file))))
    (call-with-output-file file msg)))
(write-log "scm.log")
