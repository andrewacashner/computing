; bibsort.scm
; Andrew A. Cashner, 2018/09/26

; maybe use string-split?
; TODO: newlines not being preserved in output

(use-modules 
  (srfi srfi-1))

(define sublist-test
  (lambda (ls start end)
    (and
      (list? ls)
      (not (eq? #f (memq start ls)))
      (or (not (eq? #f (memq end (memq start ls)))) 
          (and (eq? start end) 
               (not (eq? #f (memq end (cdr (memq start ls))))))))))

(define sublist
  (lambda (ls start end . mode)
    (if (sublist-test ls start end)
        (let* ([mode (if (null? mode) #f (car mode))]
               [from-start (memq start ls)]
               [after-start (cdr from-start)]
               [tmp1 (if (or (eq? mode 'trim-before) 
                             (eq? mode 'trim-both))
                         after-start
                         from-start)]
               [measure-ls (if (eq? start end)
                               (memq end after-start)
                               (memq end from-start))]
               [head-length (if (eq? #f measure-ls)
                                (length tmp1)
                                (+ 1 (- (length tmp1) (length measure-ls))))]
               [through-end (list-head tmp1 head-length)]
               [to-end (delete (last through-end) through-end)]
               [tmp2 (if (or (eq? mode 'trim-after) 
                             (eq? mode 'trim-both))
                         to-end
                         through-end)])
          tmp2)
        #f)))

(define bibkey
  (lambda (ls)
    (sublist ls #\{ #\, 'trim-both)))

(define bibentry
  (lambda (ls)
    (sublist ls #\@ #\@ 'trim-after)))

(define bibpair
  (lambda (ls)
    (let ([entry (bibentry ls)])
      (if entry
          (let ([key (bibkey entry)])
            (if key 
                (cons key entry) 
                #f))
          #f))))

(define bib->ls
  (lambda (s)
    (let loop ([chars (string->list s)] [ls '()])
      (if (null? chars)
          (reverse ls)
          (let ([pair (bibpair chars)])
            (if pair
                 (let* ([biblength (length (cdr pair))]
                        [rest (list-tail chars biblength)])
                   (loop rest (cons pair ls)))
                 (loop (cdr chars) ls)))))))
            

(define alist-key-strcmp?
  (lambda (ls1 ls2)
    (let ([key1 (list->string (car ls1))]
          [key2 (list->string (car ls2))])
    (string-ci<? key1 key2))))

(define sort-keys
  (lambda (s)
    (let ([pairs (bib->ls s)])
    (sort-list pairs alist-key-strcmp?))))

(define bibsort
  (lambda (s)
    (let loop ([ls (sort-keys s)] [strings '()])
      (if (null? ls)
          (string-join (map list->string (reverse strings)))
          (loop (cdr ls) (cons (cdar ls) strings))))))

(define bib1 "\
@Book{Cashner:HearingFaith,
 author={Cashner, Andrew A.},
 title={Hearing Faith},
 year=2020
}
")

(define bib2 "\
@Article{Cashner:Cards,
  author={Cashner, Andrew A.},
  title={Playing Cards at the Eucharistic Table},
  year=2014
}
")

(define bib (string-append bib1 bib2))

