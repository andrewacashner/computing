#!/usr/bin/env sh 
exec guile -e main -s "$0" "$@"
!#

;; work.scm -- Get to work on one of your projects
;; Andrew A. Cashner, 2018/06/15

(use-modules 
  (srfi srfi-1))

(define Default-file "/home/andrew/scm/work/projects.scm")

(define alist->keylist
  (lambda (ls)
    "Given alist, make list of just the keys"
    (fold-right 
      (lambda (this acc) 
        (cons (car this) acc)) 
      '() ls)))

(define enumerate-list
  (lambda (ls)
    "Add consecutive numbers to items of list"
    (fold
      (lambda (this acc)
        (let* ([count (+ 1 (length acc))]
               [text (format #f "~a. ~a" count this)])
        (cons text acc)))
      '() ls)))

(define list-projects
  (lambda (ls)
    "Make single string of numbered items from list of strings"
    (let* ([keys (alist->keylist ls)]
           [enumerated (reverse (enumerate-list keys))])
      (string-join enumerated "\n" 'suffix))))

(define get-project-dir
  (lambda (ls n)
    (let ([project (list-ref ls (- n 1))]) 
      (cdr project))))

(define main
  (lambda (args)
    "Allow user to select from numbered list of projects, then cd to the
    directory of that project."
    (let* ([default Default-file] 
           [file (cond [(= 1 (length args)) default] 
                       [(> 1 (length args)) (cadr args)])] 
           [projects (with-input-from-file file read)]
           [project-list (list-projects projects)])
      (begin
        (format #t "Select project:\n~a" project-list)
        (let* ([n (read)]
               [dir (get-project-dir projects n)])
          (system (format #f "cd ~a; exec bash" dir)))))))






