\version "2.23"

% Function 'makebook': Make a \book.
%   INPUT:  title (Lilypond \markup expression), 
%           music (Lilypond music expression inside curly braces {})
%           words (Lilypond \lyricmode expression)
%   RETURNS: A \book expression with the given contents.
makebook = 
#(define-scheme-function 
   (title music words)
   (markup? ly:music? ly:music?)
      #{
\bookpart { 
    \header { title = $title } 
    \score { 
        \new Staff 
          <<
            \new Voice { $music }
            \new Lyrics { $words }
          >>
        }
    }
#})

% Lilypond-centered approach 
%title = \markup "title1"
%music = { c'4 c'4 c'4 c'4 } 
%words = \lyricmode { this is some text }
%
%% Call the Scheme function with the Lilypond variables defined above
%#(makebook title music words)


#(define songs
   '(((title . #{ \markup { "title1" } #})
      (music . #{ c'4 c'4 #})
      (words . #{ \lyricmode { word one } #}))
     ((title . #{ \markup { "title2" } #})
      (music . #{ d'4 d'4 #})
      (words . #{ \lyricmode { word two } #}))
     ((title . #{ \markup { "title3" } #})
      (music . #{ e'4 e'4 #})
      (words . #{ \lyricmode { word three } #}))))

#(define makeSongs 
   (lambda (alist)
     (map (lambda (song) 
            (let ((title (assq-ref song 'title))
                   (music (assq-ref song 'music))
                   (words (assq-ref song 'words)))
                (makebook title music words))) alist)))

#(makeSongs songs)
