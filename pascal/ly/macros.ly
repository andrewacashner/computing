MusicSoprano = { c''4 d''4 es''4 }
This is not a macro
LyricsSoprano = \lyricmode { ly -- ric text }
\new Voice = "S" { \MusicSoprano }
\new Lyrics \lyricsto "S" { \LyricsSoprano }
MusicBass = { c8 d8 e8 f8 g2 g,2
  c4 e4 f4 g4 c1 }

MusicAc = { c1 c1 \MusicBass }
\new Staff = "Basses"
  <<
    \new Voice = "B1" { \voiceOne \MusicBass }
    \new Voice = "B2" { \voiceTwo \MusicAc }
  >>

LevelOne = { \MusicSoprano }
LevelTwo = { \LevelOne }
LevelThree = { \LevelTwo }
{ \LevelThree }

