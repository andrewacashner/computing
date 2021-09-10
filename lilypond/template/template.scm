(define make-book
  (lambda (title music words)
    "Return a string with the Lilypond code for a book, given title, music, and lyrics"
    (string-append
      "\\book { \\header { title = \\markup \"" title "\" }\n" 
      "\\score { \\new Staff << \\new Voice { " music " }\n" 
      "\\new Lyrics { \\lyricmode { " words " } >> } }\n")))

(define get-song
  (lambda (alist) 
    "Call make-book with the song data from an association list"
    (let ((title (assq-ref alist 'title)) 
          (music (assq-ref alist 'music)) 
          (words (assq-ref alist 'words))) 
      (make-book title music words))))

(define make-songs
  (lambda (ls)
    "Return a string with the Lilypond code for a given list of songs"
    (let ((song-strings (map get-song ls)))
      (string-concatenate song-strings))))

(define songs
  '(((title . "title1")
     (music . "c'4 c'4")
     (words . "word one"))
    ((title . "title2")
     (music . "d'4 d'4")
     (words . "word two"))
    ((title . "title3")
     (music . "e'4 e'4")
     (words . "word three"))))

(format #t (make-songs songs))
