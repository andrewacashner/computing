(define new-pet
  (lambda (type)
    (lambda (name)
      (cons type name))))

(define pet (new-pet 'default))
(define dog (new-pet 'dog))
(define cat (new-pet 'cat))

(define pet-type car)
(define pet-name cdr)

(define pet-sounds
  '((dog . "Woof")
    (cat . "Meow")
    (default . "?")))

(define pet-says
  (lambda (pet)
    (assq-ref pet-sounds (pet-type pet))))

;; Another approach
; (define any-pet
;   (lambda (pet-name pet-says)
;     `((name ,pet-name)
;       (says ,pet-says))))
; 
; (define pet-type
;   (lambda (says)
;     (lambda (name) (any-pet name says))))
; 
; (define pet (pet-type "?"))
; (define dog (pet-type "Woof"))
; (define cat (pet-type "Meow"))
; 
; (define pet-says
;   (lambda (pet)
;     (car (assq-ref pet 'says)))) 
; ; car is necessary because of quasiquote situation above
; 
; (define pet-name
;   (lambda (pet)
;     (car (assq-ref pet 'name))))

(define pet-greeting
  (lambda (pet)
    (format #f "~a says \"~a\"!" (pet-name pet) (pet-says pet))))

(let* ([dog-names '("Bowser" "Rex" "Admiral McSir")]
       [cat-names '("Fluffy" "Tiny" "Madame Clawe")]
       [dogs (map dog dog-names)]
       [cats (map cat cat-names)]
       [pets (apply append (list dogs cats))]
       [pets (cons (pet "Baron von Fiend") pets)]) ; shuffle
  (display (string-join (map pet-greeting pets) "\n"))
  (newline))


