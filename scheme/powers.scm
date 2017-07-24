(define powers
  (lambda (n max)
    "Given two integers (a base value and a maximum value),
    return a list of the powers of the base value less than the maximum"
      (letrec
        ([inner-powers
           (lambda (current)
             (if (> current max)
               '()
               (cons current (inner-powers (* n current)))))])
        (inner-powers n))))

