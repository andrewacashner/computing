(use-modules (srfi srfi-1))

(define register-letters
  (lambda (str dict increment)
    (fold (lambda (c acc) 
            (let* ([entry (assoc c dict)]
                   [count (if entry 
                            (+ increment (cdr entry))
                            increment)])
              (alist-cons c count acc)))
          '()
          (string->list str))))

(define is-permutation
  (lambda (base-str compare-str)
    (let* ([base-letters (register-letters base-str '() 1)]
           [comparison (register-letters compare-str base-letters -1)])
      (every (lambda (pair) (= 0 (cdr pair))) comparison))))

(let* ([test-values '(("abba" . "baab") 
                      ("dad" . "add") 
                      ("bacon" . "eggs") 
                      ("shut" . "tush"))]
       [test (lambda (pair) 
               (let* ([s1 (car pair)]
                      [s2 (cdr pair)]
                      [result (is-permutation s1 s2)])
                 (format #f "~s vs ~s: ~a" s1 s2 result)))]
       [test-results (string-join (map test test-values) "\n" 'suffix)])
  (display test-results))

