;; guile-dbi test 2018/04/13

(use-modules 
  (shell filenames)
  (ice-9 rdelim)
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

(define ls->db-values
  (lambda (ls db table)
    (if (null? ls)
        0
        (begin
          (new-values db table (car ls))
          (ls->db-values (cdr ls) db table)))))

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

(define line->values
  (lambda (str)
    (let ([str (open-input-string str)])
    (let loop ([value (read-delimited "," str)] [ls '()])
      (if (eof-object? value)
          (reverse ls)
          (loop (read-delimited "," str) (cons value ls)))))))


(define file->lines
  (lambda (infile)
    (let loop ([line (read-line infile)] [ls '()])
      (if (eof-object? line)
          (reverse ls)
          (loop
            (read-line infile)
            (cons line ls))))))

(define csv->db
  (lambda (infile dbname table)
    "Create new database and insert csv rows as values into table"
    (let* ([db (new-db dbname)]
           [csv-lines (call-with-input-file infile file->lines)]
           [table-spec (car csv-lines)]
           [data (cdr csv-lines)])
      (begin
        (new-table db table table-spec)
        (ls->db-values data db table)
        (dbi-close db)))))

;; FYI This basically duplicates the functionality of sqlite3:
;; sqlite> .mode csv
;; sqlite> .import file table
;; Except that sqlite3 cannot handle quoted CSV fields!
