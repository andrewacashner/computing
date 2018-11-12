; interval.scm
; Andrew A. Cashner
; 2018/11/12
;
; Given two voices in MEI XML, calculate the interval (reduced to a ninth or
; less) between the voices.

(use-modules
  (sxml simple)
  (sxml xpath)
  (rnrs enums))

(define mei
  (lambda (filename)
    (call-with-input-file 
      filename 
      (lambda (f) 
        (xml->sxml f)))))

(define diad
  (lambda (sxml)
    (let* ([path '(// measure // note // @ pname // *text*)] 
           [notes ((sxpath path) sxml)])
      (map string->symbol notes))))

(define pitches-diatonic
  (make-enumeration '(a b c d e f g)))

(define interval
  (lambda (n1 n2)
    (let* ([get-pitch (enum-set-indexer pitches-diatonic)]
           [p1 (get-pitch n1)]
           [p2 (get-pitch n2)])
      (+ 1 (- p2 p1)))))
    
