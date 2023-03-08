(define encode
  (lambda (encoding message)
    (string-map (lambda (c) (encode-char encoding c)) message)))

(define encode-char
  (lambda (encoding char)
    (let ([match (assq-ref encoding char)])
      (if match match char))))

(define decode
  (lambda (encoding message)
    (let ([dictionary (alist-flip encoding)])
      (encode dictionary message))))

(define alist-flip
  (lambda (ls)
  (map pair-flip ls)))

(define pair-flip
  (lambda (pair)
    (cons (cdr pair) (car pair))))

(define alphanum
  '((#\a . #\0)
    (#\b . #\1)
    (#\c . #\2)
    (#\d . #\3)
    (#\e . #\4)
    (#\f . #\5)
    (#\g . #\6)
    (#\h . #\7)
    (#\i . #\8)
    (#\j . #\9)
    (#\k . #\a)
    (#\l . #\b)
    (#\m . #\c)
    (#\n . #\d)
    (#\o . #\e)
    (#\p . #\f)
    (#\q . #\g)
    (#\r . #\h)
    (#\s . #\i)
    (#\t . #\j)
    (#\u . #\k)
    (#\v . #\l)
    (#\w . #\m)
    (#\x . #\n)
    (#\y . #\o)
    (#\z . #\p)))

(define backwards
  '((#\a . #\z)
    (#\b . #\y)
    (#\c . #\x)
    (#\d . #\w)
    (#\e . #\v)
    (#\f . #\u)
    (#\g . #\t)
    (#\h . #\s)
    (#\i . #\r)
    (#\j . #\q)
    (#\k . #\p)
    (#\l . #\o)
    (#\m . #\n)
    (#\n . #\m)
    (#\o . #\l)
    (#\p . #\k)
    (#\q . #\j)
    (#\r . #\i)
    (#\s . #\h)
    (#\t . #\g)
    (#\u . #\f)
    (#\v . #\e)
    (#\w . #\d)
    (#\x . #\c)
    (#\y . #\b)
    (#\z . #\a)))

(define (alnum msg) 
  (encode alphanum msg))

(define (numal msg) 
  (decode alphanum msg))

(define (backward msg) 
  (encode backwards msg))

(define (forward msg) 
  (decode backwards msg))

