;; guile-dbi test 2018/04/13

(use-modules 
  (ice-9 format)
  (dbi dbi))

(define new-db
  (lambda (str)
    (dbi-open "sqlite3" str)))

(define new-table
  (lambda (db name spec)
    (dbi-query db (format #f "CREATE TABLE IF NOT EXISTS ~a (~a)" name spec))))

(define new-values
  (lambda (db table spec)
    (dbi-query db (format #f "INSERT OR IGNORE INTO ~a VALUES (~a)" table spec))))

(define display-query
  (lambda (db query)
    (begin
      (dbi-query db query)
      (display (get-all-rows db))
      (newline))))

(define get-all-rows
  (lambda (db)
    (let loop ([ls '()]
               [this-row (dbi-get_row db)])
      (if (eq? #f this-row)
          (reverse ls)
          (loop (cons this-row ls) (dbi-get_row db))))))

(define ls->query
  (lambda (ls)
    (let make-str ([data ls] [str-ls '()]) 
      (if (null? data)
        (string-join (reverse str-ls) ",")
        (let* ([pair (car data)]
               [value (format #f "'~a'" (cdr pair))]) 
          (make-str (cdr data) (cons value str-ls)))))))

(define alist->db-values
  (lambda (ls db table)
    (if (null? ls)
        0
        (let* ([value-str (ls->query (car ls))])
        (begin 
          (new-values db table value-str)
          (alist->db-values (cdr ls) db table))))))

(define new-data
  (lambda (ls)
    (let ([keys (car ls)])
      (let row ([ls (cdr ls)] [data '()])
        (if (null? ls)
            (reverse data)
            (let ([new-row (row->alist keys (car ls))])
              (row (cdr ls) (cons new-row data))))))))

(define row->alist 
  (lambda (keys ls)
    (let loop ([keys keys] [ls ls] [alist '()])
      (if (null? ls)
          (reverse alist)
          (loop (cdr keys) (cdr ls) 
               (acons (car keys) (car ls) alist))))))


(define Data
  '(((year         . 1653)
     (city         . Madrid)
     (institution  . CapReal)
     (feast        . Nav)
     (signature    . "E-Mn: VE 989/32"))
    ((year         . 1653)
     (city         . Seville)
     (institution  . Cat)
     (feast        . Reyes)
     (signature    . "E-Mn: VE 834/12b"))))

(define ShortData
  '((year city institution feast signature)
    (1653 Madrid CapReal Nav "E-Mn: VE 989/32")
    (1653 Seville Cat Reyes "E-Mn: VE 834/12b")))

; Using data in alist format
; (let* ([db (new-db "ex3.db")]
;        [table "vcpoems"]
;        [table-spec "year integer, city, institution, feast, signature"]
;        [data Data])
;   (begin
;     (new-table db table table-spec)
;     (alist->db-values data db table)
;     (display-query db "SELECT * FROM vcpoems")
;     (dbi-close db)))

; Using data in list of lists with alist keys as first list
(let* ([db (new-db "ex3.db")]
       [table "vcpoems"]
       [table-spec "year integer, city, institution, feast, signature"]
       [data (new-data ShortData)])
  (begin
    (new-table db table table-spec)
    (alist->db-values data db table)
    (display-query db "SELECT * FROM vcpoems")
    (dbi-close db)))


