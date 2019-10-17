; \MultiLineMarkup "one" "two" "three"

(use-modules
  (ice-9 format))

(define ly
  (lambda (cmd arg) 
    (format #f "\\~a { ~a }" cmd arg)))

(define str-arg 
  (lambda (str) 
    (format "~s" str)))

(define multiline-markup
  (lambda ls
    (let* ([lines (map (lambda (str) (ly 'line (str-arg str))) ls)]
           [lines (string-join lines)])
      (ly 'markup (ly 'column lines)))))

         

