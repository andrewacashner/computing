\version "2.28"

% Mozart Clarinet Quintet
\header {
  composer = "Wolfgang Amadé Mozart"
  title = "Quintet for Clarinet and Strings in A Major"
}

MusicViolinI = {
  \clef "treble"
  \time 4/4
  \key a\major
  | e''2 cis''2
  | b''2 a''2
}

\score {
  <<
    \new Staff
    <<
      \new Voice { \MusicViolinI }
    >>
  >>
}

