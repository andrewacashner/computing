(use-modules (dbi dbi))

(define db-obj (dbi-open "sqlite3" "example1.db"))

(dbi-query db-obj "create table hellotable(id int, name varchar(15))")

(display db-obj) (newline)

(dbi-query db-obj "insert into hellotable ('id', 'name') values ('33', 'ola')")
(dbi-query db-obj "insert into hellotable ('id', 'name') values ('34', 'dzien dobre')")
(display db-obj) (newline)

(dbi-query db-obj "select * from hellotable")
(display db-obj) (newline)
(write (dbi-get_row db-obj)) (newline)
(write (dbi-get_row db-obj)) (newline)

(dbi-close db-obj)
(display db-obj) (newline)

