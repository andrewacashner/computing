\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{ririmba}[2020/03/06 
Make PDF and PPTX slides of song lyrics and texts for church services]

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{beamer}}
\ProcessOptions\relax
\LoadClass[17pt]{beamer}
\setbeamersize{text margin left=1cm, text margin right=1cm}

\RequirePackage{xparse}
\RequirePackage{etoolbox}
\RequirePackage{environ}

% TYPESETTING OPTIONS

\RequirePackage[none]{hyphenat}
\RequirePackage[document]{ragged2e}
\frenchspacing
\sloppy

% FONTS
\NewDocumentCommand{\songtextsize}{}{\large}
\NewDocumentCommand{\bibletextsize}{}{\small}

% SONG LYRIC SLIDES
% Store song metadata and print it in the right places

% Create a new private variable 
% #1 optional command to be applied to argument in private variable,
%    or text to be put before it
% #2 variable name
% NB - this does not initialize the private variable.
%
%  E.g., if \newVariable{name} is given, then command \name{Francisco} will be
%  created that sets private variable \@name to `Francisco'.
%  E.g., if \newVariable[\texbf]{VIPname} is given, command \bigname{Francisco}
%  will set private variable \@bigname to \textbf{Francisco}
\NewDocumentCommand{\newVariable}{ O{} m }{%
    \expandafter\NewDocumentCommand\csname#2\endcsname{ m }{%
        \csdef{@#2}{#1{##1}}%
    }%
}

% Print command only if it is not \empty
\RequirePackage{xifthen}
\NewDocumentCommand{\printifset}{ m }{%
    \ifthenelse{\isempty{\csuse{#1}}} {} {\csuse{#1}}%
}

% Set up our variables
\newVariable{songtitle}
\newVariable{creator}
\newVariable{legal}

\newVariable[\printccli]{ccli}
\NewDocumentCommand{\printccli}{ m }{CCLI $\#{#1}$}

% Format the song data for printing at the bottom of the slide
\NewDocumentCommand{\@footcredits}{}{%
    \par
    \vspace*{\fill}%
    \bgroup\tiny
    \begin{tabular}{p{\linewidth}}
        \@creator \\
        \@legal \\
        \printifset{@ccli}
    \end{tabular}%
    \egroup
}

% SONG LYRICS: Store and recall frames in a specified order
% Store data for song saved by \songframe commands and reprint in order requested
% by \verseorder command
\NewDocumentEnvironment{song}{ m }{%
    \setcounter{slide}{0}
    \songtitle{#1}%
    \centering
    \songtextsize
}{%
    \@verseorder
}

% Save a section of lyrics in the command csname #1
\newtoks\tmptoks
\NewEnviron{songframe}[1]{%
    \tmptoks=\expandafter{\BODY}%
    \global\csedef{#1}{\the\tmptoks}
}

% Make the slides
% Only put title and credits on the first slide
\newcounter{slide}
\NewDocumentCommand{\@slide}{ m }{%
    \stepcounter{slide}
    \ifnum\value{slide} = 1
        \begin{frame}
            \section{\@songtitle}
            \frametitle{\@songtitle}
            \subsection{#1}
            \begin{verse}
                \csuse{#1}
            \end{verse}
            \@footcredits
       \end{frame}%
    \else
        \subsection{#1}
        \begin{frame}
            \begin{verse}
                \csuse{#1}
            \end{verse}
        \end{frame}
    \fi
}

% Set the order for songframe commands
\NewDocumentCommand{\verseorder}{ > { \SplitList {,} } m }{%
    \csgdef{@verseorder}{%
        \ProcessList{#1}{\@slide}%
    }%
}

% BIBLE TEXTS (and other texts)
% Similar to songs but without being able to reorder the frames;
% they are just printed in the order entered like normal frames.
% Put the whole text in a `bibletext' environment; the argument is the title.
% You can use regular beamer frames of course, but the `verseframe'
% environment makes it verse by default and automatically adds the title to
% the first slide.
\newVariable{texttitle}
\newcounter{textframe}

\NewDocumentEnvironment{bibletext}{ m }{%
    \section{#1}
    \texttitle{#1}
    \setcounter{textframe}{0}
    \centering
    \bibletextsize
}{}

\NewEnviron{verseframe}{%
    \stepcounter{textframe}
    \tmptoks=\expandafter{\BODY}%
    \begin{frame}
        \ifnum\value{textframe} = 1
            \frametitle{\@texttitle}
        \fi
        \begin{verse}
            \the\tmptoks
        \end{verse}
    \end{frame}%
}

% GRAPHICS BACKGROUND
\RequirePackage{tikz}
\RequirePackage{graphics}
\graphicspath{ {img/} }

% Bring in a background image from the `img' directory as a transparent
% background

% Shortcuts for Beamer formatting
% #1 optional sizing commands for \includegraphics
%    default: width fits full slide
% #2 optional opacity level
%    default: 1 (not at all); you might want 0.3 (mostly transparent)
% #3 filename of image
\NewDocumentCommand{\setbackground}{ O{ 
        width=\paperwidth, 
        height=\paperheight, 
        keepaspectratio
    } O{1} m }{%
    \setbeamertemplate{background}{%
        \begin{tikzpicture}[remember picture, overlay]
            \node[opacity=#2,at=(current page.center)]{%
                \includegraphics[#1]{#3}%
            };
        \end{tikzpicture}%
    }%
}

% IMPORT FILES
\NewDocumentCommand{\usetemplate}{ m }{\input{templates/#1}}
\NewDocumentCommand{\inputtext}  { m }{\input{library/texts/#1}}

% Take an argument and remove it from output 
\NewDocumentCommand{\gulp}{ m }{}

% Set \@verseorder to #1 and then make \verseorder do nothing
% so that this overrides what's set in the song environment
\NewDocumentCommand{\overrideVerseorder}{ m }{%
    \verseorder{#1}%
    \let\verseorder\gulp
}

% Input song
% #1 optional: new arguments to \verseorder (select or rearrange verses)
% #2 filename
\NewDocumentCommand{\inputsong}{ o m }{%
    \begingroup
    \IfValueTF{#1} {\overrideVerseorder{#1}} {}%
    \input{library/songs/#2}%
    \endgroup
}

\endinput
