% SALAZAR: ANGELICOS COROS
% MUSICAL EXAMPLE FOR DISS CH. 1
% CLIP 1

\version "2.18.2"
\include "../ly/phd.ly"
\include "../ly/example.ly"

%****************************************
% MUSIC: CLIP 1 (mm. 1--9)
%****************************************

% CHORUS I

MusicSIi = \relative c' {
	\CantusMollis
	\time 6/2
	\clef "treble"
	r2 r f c'2. c4 bes2 |
	a2. g4 f2 g g c |
	c1 c2 c1.~ \i |
	c2 c1 \o c2 r2 r |

	% m. 5
	R\breve. |
	R\breve. |
	r2 r a c1 bes2 |
	a1 a2 c1 c2 |
	c1 a2 g1 g2 |

}

MusicSIii = \relative c' {
	\CantusMollis
	\clef "treble"
	% METER 6/2
	R\breve. |
	r2 r f c'2. c4 bes2 |
	a2. g4 f2 g2. a4 bes2 |
	a1 a2 g1 c2 |

	% m. 5
	c2 r r r1. |
	R\breve. |
	r2 r c2 c1 c2 |
	c1 a2 g1 g2 |
	a2 \i c1 \o c1 c2 |
}

MusicAI = \relative c' {
	\CantusMollis
	\clef "treble"
	% METER 6/2	
	R\breve. |
	r2 r f e2. d4 e2 |
	f2. c4 d2 e2. f4 g2 |
	f1 f2 g1 g2 |

	% m. 5
	f2 r r r1. |
	R\breve. |
	r2 r f g1 g2 |
	f1 c2 e1 d2 |
	c1 f2 e1 e2 |
}

% CHORUS II

MusicSII = \relative c' {
	\CantusMollis
	\clef "treble"
	% METER 6/2
	R\breve.*4 |
	
	% m. 5
	r2 r f c'2. c4 bes2 |
	a2. g4 f2 g2. a4 bes2 |
	a1 c2 c1 c2 |
	c\breve \i c1 \o |
	c1 c2 c1. |
}

MusicAII = \relative c' {
	\CantusMollis
	\clef "treble"
	% METER 6/2
	R\breve.*4 |

	% m. 5
	r2 r f2 e2. d4 e2 |
	f2. c4 d2 e2. f4 g2 |
	f1 c2 e1 d2 |
	c1 f2 g1 g2 |
	f1 c2 e1. |
}

MusicTII = \relative c {
	\CantusMollis
	\clef "treble_8"
	% METER 6/2	
	R\breve.*3 |
	r2 r f c'2. c4 bes2 |

	% m. 5
	a2. g4 f2 g g c |
	c\breve \i c1 \o |
	c1 a2 g1 g2 |
	a1 c2 c1 bes2 |
	a1 f2 g1. |
}

MusicBII = \relative c {
	\CantusMollis
	\clef "bass"
	% METER 6/2
	r2 r f2_\markup \bold "Ac." e2. d4 e2 |
	f2. e4 d2 c c c |
	f,1 f2 c'1 c2 |
	f,1 f'2^\markup \bold "T./B." 
		<< { \voiceOne e2. d4 e2 } \new Voice { \voiceTwo c1 c2 } >> |
	
	% m. 5
	\oneVoice \voiceTwo f2._\markup \bold "B., Ac." e4 d2 c c c |
	f,1 f2 c'1 c2 |
	f,1 f'2 c1 c2 |
	f2. e4 d2 c c c |
	f,1 f'2 c1 c2 |
}

%****************************************
% LYRICS: CLIP 1
%****************************************

% CHORUS I

LyricsSIi = \lyricmode {
	An -- gé -- li -- cos 
	co -- ros con go -- zo can -- 
	tad, can -- tad, __ 
	can -- tad, 
	
	% m. 5
	% 2mm rest
	can -- tad, \it can -- 
	tad, can --  tad, \rm can --
	tad, \it can -- tad \rm la 
}

LyricsSIii = \lyricmode {
	% 1m rest
	An -- gé -- li -- cos
	co -- ros con go -- zo can -- 
	tad, can -- tad, \it can -- 
	
	% m. 5
	tad, \rm
	% 1m rest
	can -- tad, \it can -- 
	tad, \rm can -- tad, \it can --
	tad, \rm can -- tad la 
}

LyricsAI = \lyricmode {
	% 1m rest
	An -- gé -- li -- cos 
	co -- ros con go -- zo can -- 
	tad, can -- tad, \it can --

	% m. 5
	tad, \rm
	% 1m rest
	can -- tad, \it can --
	tad, \rm can -- tad, \it can --
	tad, \rm can -- tad la 
}

% CHORUS II

LyricsSII = \lyricmode {
	% 4mm rest

	% m. 5
	An -- gé -- li -- cos
	co -- ros con go -- zo can --
	tad, can -- tad, \it can --
	tad, can -- 
	tad, can -- tad. \rm

	% m. 10
	% 1m rest
	la
	glo -- ria~a Be -- lén, que~es ca -- 
	sa de pan. 
	% 1m  rest
}

LyricsAII = \lyricmode {
	% 4mm rest

	% m. 5
	An -- gé -- li -- cos
	co -- ros con go -- zo can --
	tad, can -- tad, \it can --
	tad, \rm can -- tad, \it can --
	tad, can -- tad, \rm

	% m. 10
	% 1m rest
	la
	glo -- ria~a Be -- lén, que~es ca --
	sa de pan.
	% 1m rest
}

LyricsTII = \lyricmode {
	% 3mm rest
	An -- gé -- li -- cos

	% m. 5
	co -- ros con go -- zo can --
	tad, can --
	tad, can -- tad, \it can --
	tad, can -- tad, can --
	tad, can -- tad, \rm
}

%****************************************
% SCORE: CLIP 1
%****************************************

\score {
	<<
		\new ChoirStaff
			<<
				\new Staff = "SIi"
				<<
					\Instrument #"Ti. I-1" #""
					\new Voice = "SIi" { \MainStyle \MusicSIi }
					\new Lyrics \lyricsto "SIi" { \LyricsSIi }
				>>
				\new Staff = "SIii-AI"
				<<
					\Instrument
						\TwoLineName #"Ti. I-2" #"A. I"
						#"" 
					\new Voice = "SIii" { \voiceOne \MainStyle \MusicSIii }
					\new Voice = "AI" { \voiceTwo \MainStyle \MusicAI }
					\new Lyrics \lyricsto "AI" { \LyricsAI }
				>>
			>>
		\new ChoirStaff
			<<
				\new Staff = "SAII"
				<<
					\Instrument
						\TwoLineName #"Ti. II" #"A. II"
						#""
					\new Voice = "SII" { \voiceOne \MainStyle \MusicSII }
					\new Voice = "AII" { \voiceTwo \MainStyle \MusicAII }
				
					\new Lyrics \with { alignAboveContext = #"SAII" }
						\lyricsto "SII" { \LyricsSII }
					\new Lyrics \lyricsto "AII" { \LyricsAII }
				
				>>	
				\new Staff = "TBII"
				<<
					\Instrument
						\TwoLineName #"T. II" #"B. II, Ac."
						#""
					\new Voice = "TII" { \voiceOne \MainStyle \MusicTII }
					\new Voice = "BII-Ac" { \voiceTwo \MainStyle \MusicBII }
				
					\new Lyrics \with { alignAboveContext = #"TBII" }
						\lyricsto "TII" { \LyricsTII }
				>>		
		>>
	>>
	\layout {
		\LayoutStyle
	}
}
					

