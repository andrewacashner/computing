(use-modules
  (srfi srfi-1))

(define encode
  (lambda (encoding message)
    "Encode a message in the given encoding 
    (alist, can map characters or strings)"
    (if (char? (caar encoding))
      (encode-chars encoding message)
      (encode-strings encoding message))))

(define decode 
  (lambda (encoding message)
    "Decode a message in the given encoding"
    (if (char? (caar encoding))
      (decode-chars encoding message)
      (decode-strings encoding message))))

(define encode-one
  (lambda (encoding item)
  "Return the char matching a given char in a given encoding (alist)"
    (let ([match (assoc-ref encoding item)])
      (if match match item))))

(define encode-chars
  (lambda (encoding message)
  "Encode a string in a given encoding.
    Input: (1) alist mapping single characters
           (2) string to be encoded
    Output: The encoded string"
    (string-map (lambda (c) (encode-one encoding c)) 
                (string-downcase message))))

(define encode-strings
  (lambda (encoding message)
  "Encode a string in a given encoding.
    Input: (1) alist mapping strings
           (2) string to be encoded
    Output: The encoded string"
    (let ([input (string->list (string-downcase message))])
      (string-join
        (fold-right 
          (lambda (c acc) 
            (cons (encode-one encoding (string c)) acc)) 
        '() input)))))

(define decode-chars
  (lambda (encoding message)
  "Like 'encode-chars', decode a string in a given char-char encoding. Reverse
    the given alist to look up values instead of keys."
    (let ([dictionary (alist-flip encoding)])
      (encode dictionary message))))

(define decode-strings
  (lambda (encoding message)
  "Like 'encode-strings', decode a string in a given string-string encoding.
    Reverse the given alist to look up values instead of keys."
    (let* ([dictionary (alist-flip encoding)]
           [input (string-split message #\space)]
           [output (fold-right (lambda (s acc)
                                 (cons (encode-char dictionary s) acc))
                               '() input)])
      (string-concatenate output))))

(define alist-flip
  (lambda (ls)
  "Flip all the key/value pairs of an association list so that (a . b) becomes
  (b.  a)"
  (map pair-flip ls)))

(define pair-flip
  (lambda (pair)
  "Flip a single pair so that (a . b) becomes (b . a)"
    (cons (cdr pair) (car pair))))

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


(define alphanum
  '((" " . "-")
    ("a" . "0")
    ("b" . "1")
    ("c" . "2")
    ("d" . "3")
    ("e" . "4")
    ("f" . "5")
    ("g" . "6")
    ("h" . "7")
    ("i" . "8")
    ("j" . "9")
    ("k" . "10")
    ("l" . "11")
    ("m" . "12")
    ("n" . "13")
    ("o" . "14")
    ("p" . "15")
    ("q" . "16")
    ("r" . "17")
    ("s" . "18")
    ("t" . "19")
    ("u" . "20")
    ("v" . "21")
    ("w" . "22")
    ("x" . "23")
    ("y" . "24")
    ("z" . "25")))

(define beetle 
  '((#\a . #\u)
    (#\b . #\v)
    (#\c . #\w)
    (#\d . #\x)
    (#\e . #\y)
    (#\f . #\z)
    (#\g . #\a)
    (#\h . #\b)
    (#\i . #\c)
    (#\j . #\d)
    (#\k . #\e)
    (#\l . #\f)
    (#\m . #\g)
    (#\n . #\h)
    (#\o . #\i)
    (#\p . #\j)
    (#\q . #\k)
    (#\r . #\l)
    (#\s . #\m)
    (#\t . #\n)
    (#\u . #\o)
    (#\v . #\p)
    (#\w . #\q)
    (#\x . #\r)
    (#\y . #\s)
    (#\z . #\t)))

(define bug
  '((" " . "-")
    ("a" . "14")
    ("b" . "15")
    ("c" . "16")
    ("d" . "17")
    ("e" . "18")
    ("f" . "19")
    ("g" . "20")
    ("h" . "21")
    ("i" . "22")
    ("j" . "23")
    ("k" . "24")
    ("l" . "25")
    ("m" . "26")
    ("n" . "13")
    ("o" . "12")
    ("p" . "11")
    ("q" . "10")
    ("r" . "9")
    ("s" . "8")
    ("t" . "7")
    ("u" . "6")
    ("v" . "5")
    ("w" . "4")
    ("x" . "3")
    ("y" . "2")
    ("z" . "1")))

(define to-beetlebug
  (lambda (msg)
    (encode bug (encode beetle msg))))

(define from-beetlebug
  (lambda (msg)
    (decode beetle (decode bug msg))))
