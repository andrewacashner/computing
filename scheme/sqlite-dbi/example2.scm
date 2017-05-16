(use-modules (dbi dbi))

(define (writeln msg)
  (begin (write msg) (newline)))

(define (displayln msg)
  (begin (display msg) (newline)))

(define (dbi-check-result db-obj)
  (if (eq? (car (dbi-get_status db-obj)) 0)
    #t
    (displayln db-obj)))

(define (dbi-get_row_value db-obj)
  (cdar (dbi-get_row db-obj)))

(define (dbi-get_all_row_values ls db-obj)
  (let ((this-row-value (dbi-get_row_value db-obj)))
    (if (null? this-row-value)
      '()
      (dbi-get_all_row_values (append ls this-row-value) db-obj))
    ))

(define db-obj (dbi-open "sqlite3" "example2.db"))

(dbi-query db-obj "CREATE TABLE IF NOT EXISTS composers (lastname, firstname, principalCity)")
(dbi-check-result db-obj)

(dbi-query db-obj "INSERT OR IGNORE INTO composers VALUES ('Mozart', 'Wolfgang Amadeus', 'Vienna')")
(dbi-query db-obj "INSERT OR IGNORE INTO composers VALUES ('Bach', 'Johann Sebastian', 'Leipzig')")
(dbi-check-result db-obj)

(dbi-query db-obj "SELECT * FROM composers")
(dbi-check-result db-obj)
(let [(msg [string-join (list (dbi-get_row_value db-obj) (dbi-get_row_value db-obj)) "; "])]
      (writeln msg))

(dbi-query db-obj "SELECT principalCity FROM composers ORDER BY principalCity")
(if (eq? (dbi-check-result db-obj) #t) 
  (writeln (dbi-get_row_value db-obj))
  #f)

(dbi-close db-obj)
(dbi-check-result db-obj)

