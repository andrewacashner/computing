(define multable
  (lambda (n max)
    "Given two integers for a base value and a maximum,
    return a list of multiples of the base value"
    (letrec
      ([inner-multable
         (lambda (base max current)
           (if (> current max)
             '()
             (cons current 
                   (inner-multable base max (+ base current)))))])
      (inner-multable n max n))))
