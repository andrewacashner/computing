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
\new ChoirStaff = "coro"
<<
\new Staff = "S1"
<<
\IncipitStaff "[TIPLE 1]" "TI. 1"
{
\MSclefCi
\MeterZ
\CantusMollis

{
bes'2
}

}
\new Voice = "S1" 
{
\clef "treble"
\MeterTriple
\CantusMollis

{
% 1
| r2 r2 bes'2
% 2
| d''2 d''2. d''4
% 3
| f''2. g''4 f''2
% 4
| es''2 d''2 c''2
% 5
| c''1 c''2

}

}
>>

\new Staff = "S2"
<<
\IncipitStaff "[TIPLE 2]" "TI. 2"
{
\MSclefCi
\MeterZ
\CantusMollis

{
bes'2
}

}
\new Voice = "S2" 
{
\clef "treble"
\MeterTriple
\CantusMollis

{
R1.*5 
}

}
>>

\new Staff = "S3"
<<
\IncipitStaff "[TIPLE 3]" "TI. 3"
{
\MSclefCi
\MeterZ
\CantusMollis

{
d'2
}

}
\new Voice = "S3" 
{
\clef "treble"
\MeterTriple
\CantusMollis

{
R1.*5 
}

}
>>

\new Staff = "T"
<<
\IncipitStaff "TENORE" "T."
{
\MSclefCiv
\MeterZ
\CantusMollis

{
bes2
}

}
\new Voice = "T" 
{
\clef "treble"
\MeterTriple
\CantusMollis

{
R1.*5 
}

}
>>

>>
\new ChoirStaff = "Ac"
<<
\ShowChoirStaffBracket\new Staff = "Ac"
<<
\IncipitStaff \TwoLineName "ACOMPAÑAMIENTO" "ARPA"
 "AC."
{
\MSclefFiv
\MeterZ
\CantusMollis

{
bes2
}

}
\new Voice = "Ac" 
{
\clef "bass"
\MeterTriple
\CantusMollis

{
% 1
| r2 r2 bes2
% 2
| bes2 a2 g2
% 3
| f2. es4 d2
% 4
| es2 bes2 bes2
% 5
| f1 f2

}

}
>>

>>
>>
}
