;; Trying something object-oriented, 2017/07/18
(define newbook
  (lambda (author title location publisher year)
    (list 
      (cons "author" author)
      (cons "title" title)
      (cons "location" location)
      (cons "publisher" publisher)
      (cons "year" year))))

(define bookfield
  (lambda (book field)
    (cdr (list-ref book field))))

(define author      (lambda (book) (bookfield book 0)))
(define title       (lambda (book) (bookfield book 1)))
(define location    (lambda (book) (bookfield book 2)))
(define publisher   (lambda (book) (bookfield book 3)))
(define year        (lambda (book) (bookfield book 4)))
;; wishing for something like an enum

; (define King:Stand
;   '(("author"   . "King, Stephen")
;     ("title"    . "The Stand")
;     ("location" . "New York")
;     ("publisher" . "Doubleday")
;     ("year"     . "1978")))
; OR

(define King:Stand
  (newbook 
    "King, Stephen" 
    "The Stand" 
    "New York" 
    "Doubleday" 
    "1978")) ;; but the above key-value interface seems better
;; plus you need the key

(define search-fulltext
  (lambda (book term) ; term is value (e.g., "The Stand")
    (if (member term (map cdr book))
      #t
      #f)))

(define search-field
  (lambda (book term) ; term is key.value pair (e.g., '("title" . "The Stand"))
    (if (member term book)
      #t
      #f))) ;; but how to do this across a list of multiple books?
