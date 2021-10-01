#!/usr/bin/guile \
-e main -s
!#
; Say hello to a group of people

(define folks
  '("Joseph"
    "José"
    "Josephine"
    "Giuseppe"
    "Iosephus"
    "Josefina"
    "Josephine"))

(define hellolist
  (lambda (names)
    (let ([greetings 
            (map (lambda (name) 
                   (string-append "Hello, " name "!\n")) 
                names)])
      (string-concatenate greetings))))

(define main
  (lambda (args)
    (let ([msg (hellolist folks)])
      (display msg))))




