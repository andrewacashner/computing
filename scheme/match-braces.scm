(define substring-within-braces
  (lambda (str)
    (if (and (string-contains str "{")
             (string-contains str "}"))
      (let* ([range (find-matched-braces 0 0 0 0 (string->list str))]
             [start (car range)] 
             [end (cdr range)]) 
        (substring str start end))
      str)))

(define find-matched-braces
  (lambda (brace-level index start end str-ls)
    (if (and (null? str-ls) (= 0 brace-level))
      (cons start end)
      (let ([this-char (car str-ls)])
        (cond
          [(char=? this-char #\{) 
           (let ([start (if (= 0 brace-level) (+ index 1) start)])
             (find-matched-braces 
               (+ brace-level 1) (+ index 1) start end (cdr str-ls)))]
          [(char=? this-char #\})
            (if (= 1 brace-level) 
              (find-matched-braces 0 0 start index '())
              (find-matched-braces 
                (- brace-level 1) (+ index 1) start end (cdr str-ls)))]
          [else 
            (find-matched-braces 
              brace-level (+ index 1) start end (cdr str-ls))])))))

(define text-braces "{{This} {is {the year} of {Jason's}} Bar Mitvah}!}")
(define text-nobraces "We're more excited than we should be")

; (format #f (substring-within-braces text-nobraces))
(format #f (substring-within-braces text-braces))
