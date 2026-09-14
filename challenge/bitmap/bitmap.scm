; Display binary byte values as graphics
; Andrew Cashner, 2026/09/13

(use-modules (srfi srfi-1))

(define has-bit?
  (lambda (byte position)
    (let ([value (ash #b1 position)])
      (= value (logand byte value)))))

(define byte->bit
  (lambda (byte position)
    (if (has-bit? byte position) 1 0)))

(define byte->bitlist 
  (lambda (byte)
    (let ([positions (reverse (iota 8))])
      (map (lambda (position) (byte->bit byte position)) positions))))

(define bitlist->graphic
  (lambda (bitlist)
    (let* ([bit->glyph (lambda (bit) (if (= 1 bit) "*" " "))]
           [glyphs (map bit->glyph bitlist)])
      (string-concatenate glyphs))))

(define bytelist->graphic
  (lambda (bytelist)
    (let* ([bitlists (map byte->bitlist bytelist)]
           [byte-glyphs (map bitlist->graphic bitlists)])
      (begin
        (display (string-join byte-glyphs "\n"))
        (newline)))))

(define square 
  '(#b11111111
    #b10100001
    #b10010001
    #b10001001
    #b10000101
    #b11111111))

(bytelist->graphic square)
