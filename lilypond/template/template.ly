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
\book { 
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
title = \markup "title1"
music = { c'4 c'4 c'4 c'4 } 
words = \lyricmode { this is some text }

% Call the Scheme function with the Lilypond variables defined above
#(makebook title music words)

