;; find sexp in file

(use-modules
  (rnrs io ports)
  (ice-9 regex))

(define search-file
  (lambda (filename pattern)
    (let ([infile (open-file-input-port filename)])
      (let loop ([line (get-line infile)])
        (if (not (eof-object? line))
          (let ([str (regexp-exec pattern line)])
            (if (eqv? #f str)
              (loop (get-line infile))
              (match:substring str))))))))
; don't stop looping after first match
; after find, trim off ` ` characters, then Scheme-read the function
(define float-cmd (make-regexp "`\\(insert[^`]*\\)`"))

(search-file "sample.md" float-cmd)

