% MIGUEL MATEO DE DALLO Y LANA
% LAUDATE DOMINUM A 6
% MEX-Mc: A0113
%
% EDITED BY ANDREW A. CASHNER
% 2016-02-22

\version "2.19"

\include "../ly/standard-extensions.ly"
\include "../ly/early-music.ly"

\include "music.ly"
\include "lyrics.ly"

header {
  title     = "Laudate Dominum a 6"
  composer  = "MIGUEL MATEO DE DALLO Y LANA (d. 1705)"
  editor    = "Edited by Andrew A. Cashner"
  poet      = "Psalm 116 (Vulgate)"
  source    = "Mexico City, Cathedral Archive (MEX-Mc: A0113)"
  copyright = "Copyright © 2017 Andrew A. Cashner"
}

\score {
  <<
    \new ChoirStaff = "ChI"
    <<
      \ChoirStaffName "CHORUS I"
      \new Staff = "AI"
      <<
        \IncipitStaff "ALTO I" "A. I" { \IncipitAI }
        \new Voice = "AI" { \MusicAI}
        \new Lyrics \lyricsto "AI" { \LyricsAI }
      >>
      \new Staff = "TI"
      <<
        \IncipitStaff "TENOR I" "T. I" { \IncipitTI }
        \new Voice = "TI" { \MusicTI}
        \new Lyrics \lyricsto "TI" { \LyricsTI }
      >>
    >>
    \new ChoirStaff = "ChII"
    <<
      \ChoirStaffName "CHORUS II"
      \new Staff = "SII"
      <<
        \IncipitStaff "TIPLE II" "Ti. II" { \IncipitSII }
        \new Voice = "SII" { \MusicSII}
        \new Lyrics \lyricsto "SII" { \LyricsSII }
      >>
      \new Staff = "AII"
      <<
        \IncipitStaff "ALTO II" "A. II" { \IncipitAII }
        \new Voice = "AII" { \MusicAII}
        \new Lyrics \lyricsto "AII" { \LyricsAII }
      >>
      \new Staff = "TII"
      <<
        \IncipitStaff "TENOR II" "T. II" { \IncipitTII }
        \new Voice = "TII" { \MusicTII }
        \new Lyrics \lyricsto "TII" { \LyricsTII }
      >>
      \new Staff = "BII"
      <<
        \IncipitStaff 
            \TwoLineName "BAJO II" "[instr.]"  
            "B. II"
        \new Voice = "BII" { \MusicBII }
      >>
    >>
    \new Staff = "Ac" 
    <<
      \IncipitStaff "ACOMP." "Ac."
      \new Voice = "Ac" { \MusicAc }
    >>
  >>
}


