define(makebook,
\version "2.23"
\book { 
    \header { title = $1 } 
    \score { 
        \new Staff 
          <<
            \new Voice { $2 }
            \new Lyrics { $3 }
          >>
        }
    }
)
define(mytitle, \markup "title1")
define(mymusic, { c'4 c'4 c'4 c'4 })
define(mywords, \lyricmode { this is some text })
makebook(mytitle, mymusic, mywords)

