%% Sandbox for lilypond function testing, 2016/02/06, Andrew Cashner

\version "2.18.2"

%%%%% EDITORIAL LYRICS %%%%%

EdLyrics =
  #(define-scheme-function
    (parser location text) (scheme?)
    #{
    \override Lyrics.LyricText.font-shape = #'italic
    $text
    \revert Lyrics.LyricText.font-shape
    #})

%%%%% LIGATURE BRACKETS %%%%%
Lig =
#(define-music-function
  (parser location notes) (ly:music?)
  "Put ligature bracket over a group of notes enclosed in braces"
  #{ \[ $notes \] #})


%%%%% BRACKETS FOR MENSURAL COLORATION %%%%%

%% Once command to group notes inside a TextSpanner
%% By David Nalesnik and Thomas Morley, http://lsr.di.unimi.it/LSR/Item?id=857
#(define (text-spanner-start-stop mus)
  (let ((elts (ly:music-property mus 'elements)))
   (make-music 'SequentialMusic 'elements
    (append
     (list (make-music 'TextSpanEvent 'span-direction -1))
     (reverse (cdr (reverse elts)))
     (list (make-music 'TextSpanEvent 'span-direction 1))
     (list (last elts))))))

ColorBracketLeft =
\markup { \combine
	  \draw-line #'(0 . -1)
	  \draw-line #'(1.5 . 0)
	}

ColorBracketRight =
\markup { \combine
	  \draw-line #'(0 . -1)
	  \draw-line #'(-1.5 . 0)
	}

ColorBrackets = {
  \override TextSpanner.dash-period = #0
  \override TextSpanner.bound-details.left.text = \ColorBracketLeft
  \override TextSpanner.bound-details.right.text = \ColorBracketRight
  \override TextSpanner.bound-details.left.attach-dir = #-2
  \override TextSpanner.bound-details.right.attach-dir = #2
  \override TextSpanner.staff-padding = #2
  \override TextSpanner.bound-details.left-broken.text = ##f
  \override TextSpanner.bound-details.right-broken.text = ##f
}

%% Usage: \Color { c'2 c'2 c'2 } c'1.

Color =
#(define-music-function
  (parser location music) (ly:music?)
  "Add coloration brackets as text spanner to a group of notes enclosed in braces after the command"
  #{
  \ColorBrackets
  $(text-spanner-start-stop music)
  #})

%% For single notes: \Color does not work with only one note
%% Usage: \ColorOne c'1 %% no brackets

ColorBracketLeftRight = 
\markup {
  \halign #-0.5 \raise #1.4
  \concat { \ColorBracketLeft " " \ColorBracketRight }
}

ColorOne =
#(define-music-function
  (parser location note-event)
  (ly:music?)
  "Add coloration brackets to a single note"
  (set! (ly:music-property note-event 'articulations)
   (cons (make-music 'TextScriptEvent
	  'direction 1
	  'text #{ \ColorBracketLeftRight #})
    (ly:music-property note-event 'articulations)))
  note-event)

%%%%% SECTION HEADINGS %%%%%%

Section =
#(define-scheme-function
  (parser location SectionText) (markup?)
  "Print a section title"
  #{ \mark \markup $SectionText #})



