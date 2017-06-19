;; Trying something object-oriented, 2017/07/18
(define newbook
  (lambda (citekey author title location publisher year)
    (list 
      (cons "citekey" citekey)
      (cons "author" author)
      (cons "title" title)
      (cons "location" location)
      (cons "publisher" publisher)
      (cons "year" year))))

(define bookfield
  (lambda (book field)
    (cdr (list-ref book field))))

(define citekey     (lambda (book) (bookfield book 0)))
(define author      (lambda (book) (bookfield book 1)))
(define title       (lambda (book) (bookfield book 2)))
(define location    (lambda (book) (bookfield book 3)))
(define publisher   (lambda (book) (bookfield book 4)))
(define year        (lambda (book) (bookfield book 5)))
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
    "King:Stand"
    "King, Stephen" 
    "The Stand" 
    "New York" 
    "Doubleday" 
    "1978")) ;; but the above key-value interface seems better
;; plus you need the key

(define Kircher:Musurgia
  (newbook 
    "Kircher:Musurgia"
    "Kircher, Athanasius"
    "Musurgia universalis"
    "Rome"
    ""
    "1650"))

(define library
  (list King:Stand
        Kircher:Musurgia))


(define lookup-key
  (lambda (book term) 
    (if (null? book)
      #f
      (let* ([field (car book)] 
             [key (car field)]
             [value (cdr field)])
        (if (string=? term value)
          key
          (lookup-key (cdr book) term))))))

(define lookup-value
  (lambda (book term) 
    (if (null? book)
      #f
      (let* ([field (car book)] 
             [key (car field)]
             [value (cdr field)])
        (if (string=? term key)
          value
          (lookup-value (cdr book) term))))))

(define partner
  (lambda (pair side)
    "Given pair and either car or cdr,
    Return the other part of the pair"
    (cond [(eqv? side cdr) (car pair)]
          [(eqv? side car) (cdr pair)]
          [else #f])))

(define search-field
  (lambda (book term) ; term is key.value pair (e.g., '("title" . "The Stand"))
    (if (member term book)
      #t
      #f))) ;; but how to do this across a list of multiple books?

(define book-print-fields
  (lambda (book)
    (begin
      (display 
        (apply string-append (map cdr book)))
      (newline))))

(define book-print
  (lambda (book)
    (let ([au  (author book)]
          [ti  (title book)]
          [loc (location book)]
          [pub (publisher book)]
          [yr  (year book)])
      (string-append
        "<p>" au ". <cite>" ti "</cite>. " loc 
        (if (> 0 (string-length pub))
          (string-append ": " pub)
          "")
        ", " yr ".</p>\n"))))

(define book->biblatex
  (lambda (book)
    (let ([key (citekey book)]
          [au  (author book)]
          [ti  (title book)]
          [loc (location book)]
          [pub (publisher book)]
          [yr  (year book)])
      (string-append
        "@Book{" key ",\n"
        "author={" au "},\n"
        "title={" ti "},\n"
        "location={" loc "},\n"
        "publisher={" pub "},\n"
        "year={" yr "}\n"
        "}\n\n"))))

(define strcat-format
  (lambda (ls format)
    (apply string-append (map format ls))))

(define library-print
  (lambda (bookls)
    (strcat-format bookls book-print)))

(define library->biblatex
  (lambda (bookls)
    (strcat-format bookls book->biblatex)))
