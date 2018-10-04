% Created from XML original by lirio
\version "2.19"
\include "villancico.ly"
\header {
title = \markup { "Venid, querubines alados" }
subtitle = \markup { "De nuestra Señora. A 4." }
composer = \markup { "JUAN HIDALGO (1614–1685)" }
poet = \markup { "Anonymous" }
editor = \markup { "Edited by Andrew A. Cashner" }
copyright = \markup { "Copyright © 2018 Andrew A. Cashner" }
source = \markup { \concat { "Source: " \italic "D-Mbs" ": Mss. Mus. 2897" } }
}
\score {
<<
\new ChoirStaff = "chorus"
<<
\new Staff = "SI"
<<
\InstrumentName "TIPLE 1" "Ti. I"
\new Voice = "SI" {
\clef "treble"
\MeterTriple
\CantusMollis
% S1-1
| r2 r2 bes'2 
% S1-2
| d''2 d''2. d''4 
}
>>
>>

>>
}
