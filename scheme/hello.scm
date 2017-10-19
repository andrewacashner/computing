;; hello world with message passing using alists and testing for errors
;; 2017/10/19
(define greeter
  (lambda (msg person)
    (let* ([msg-str 
             '((hi . "Hello") 
               (bye . "Goodbye"))]
           [greeting (assq-ref msg-str msg)])
      (if (eq? #f greeting) 
        'greeting:bad-msg
        (let ([result
               (punctuator 
                  'exclam
                  (punctuator 
                    'space 
                    (punctuator 
                      'comma 
                      greeting)) 
                  person)])
          (if (eq? result 'punctuator:bad-msg)
            'greeting:bad-msg-to-punctuator
            result))))))

(define punctuator
  (lambda (msg . str)
    (let* ([base-str (apply string-append str)]
           [punct-ls 
             '((space      . " ") 
               (comma      . ",") 
               (exclam     . "!")
               (period     . ".")
               (question   . "?"))]
           [punct-str (assq-ref punct-ls msg)])
      (if (eq? #f punct-str)
        'punctuator:bad-msg
        (string-append base-str punct-str)))))

;; (greeter 'hi "world") => "Hello, world!"


