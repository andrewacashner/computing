;; bibliography key-value interface
;; Andrew A. Cashner, 2017-05-26

;; (define Kircher:Musurgia
;;   (list
;;     '("type"     . "book")
;;     '("author"   . "Kircher, Athanasius")
;;     '("title"    . "Musurgia universalis")
;;     '("location" . "Rome")
;;     '("year"     . "1650")))

(define (make-bibitem type author title location year)
  (list 
    (cons "type" type)
    (cons "author" author)
    (cons "title" title)
    (cons "location" location)
    (cons "year" year)))

; (define (print-bibitem bibitem)
;   (string-append
;     "@Book{" (quote bibitem) "}")) ;; how to get procedure name as string?

(define Kircher:Musurgia 
  (make-bibitem
    "book"
    "Kircher, Athanasius"
    "Musurgia universalis"
    "Rome"
    "1650"))

(display Kircher:Musurgia)
(newline)
