\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{example1}[2016/09/28 Example 1 of changeable formatting]
\LoadClass{article}

% Available class options:
  %  palatino -- use palatino fonts with different arrangement of styles
  %  notranslation -- hide the translation in poemtranslation environment
  %  rainbowtitle -- display the title in rainbow colors!

\newif\iftranslation
\translationtrue
\DeclareOption{notranslation}{\translationfalse}

\newif\ifpalatino
\palatinofalse
\DeclareOption{palatino}{\palatinotrue}

\newif\ifrainbowtitle
\rainbowtitlefalse
\DeclareOption{rainbowtitle}{\rainbowtitletrue}

\ProcessOptions\relax


\ifpalatino
  % We can change all the font styles around with one switch.
  \RequirePackage{newpxtext,newpxmath}
  \let\textsf\textbf
  \let\oldscshape\scshape
  \let\scshape\itshape
  \let\itshape\oldscshape
\else
  \RequirePackage{Alegreya, AlegreyaSans}
\fi
\RequirePackage[T1]{fontenc}
\RequirePackage[utf8]{inputenc}
\RequirePackage{microtype}

% Compact lists
\RequirePackage{enumitem}
\setlist{noitemsep}

% We can change the section headings with one command
\RequirePackage{sectsty}
\allsectionsfont{\scshape\color{blue}}

% We can define custom commands and do anything we like with them.
% Some of these are redefined by the font switch above.
\RequirePackage{xparse}
\RequirePackage{xcolor}

\NewDocumentCommand{\code}{}{\textsf}

% Rainbowtitle option puts each letter of title in different 
% rainbow color
\ifrainbowtitle
  \def\scando{}
  \def\scan#1{\scanA#1\end}
  \def\scanA#1{\ifx\end#1\else\scando#1\expandafter\scanA\fi}

  \newcount\rainbowcolor
  \def\spectrum#1{%
    \textcolor{\getcolor{\the\rainbowcolor}}{#1}\thinspace
    \ifnum\rainbowcolor > 4
      \rainbowcolor=0
    \else
      \advance\rainbowcolor by 1
    \fi
  }

  \def\getcolor#1{\ifcase#1 red\or orange\or yellow\or green\or blue\or violet\fi}

  \def\rainbow#1{%
    \rainbowcolor=0
    \let\scando\spectrum
    \expandafter\expandafter\scan{#1}
  }

  \RenewDocumentCommand{\maketitle}{}{%
    \begin{center}
    \Huge \rainbow{\@title}
    \end{center}
    \bigskip
  }
\fi

% Fancy underlining
\RequirePackage{ulem}
\NewDocumentCommand{\keyword}{ m }{\textcolor{magenta}{\uwave{#1}}}

% Redefinable environment, including font style and spacing
\NewDocumentEnvironment{notabene}{}
  {\itshape}
  {\par\addvspace{2ex}}

% Another customizable environment, for parallel poetic text and translation
% With one switch we can turn off one of the columns
\RequirePackage[spanish,english]{babel}
\RequirePackage{multicol}

\iftranslation
  \NewDocumentEnvironment{poemtranslation}{}
    {\footnotesize\raggedright
     \begin{multicols}{2}}
    {\end{multicols}}
  \NewDocumentEnvironment{original}{}
    {\selectlanguage{spanish}
     \color{red}
     \begin{verse}}
    {\end{verse}}
  \NewDocumentEnvironment{translation}{}
    {\columnbreak
     \color{blue}
     \begin{verse}}
    {\end{verse}}
\else
  \NewDocumentEnvironment{poemtranslation}{}
    {\color{violet}}
    {}
  \NewDocumentEnvironment{original}{}
    {\selectlanguage{spanish}
     \begin{verse}}
    {\end{verse}}
  \RequirePackage{environ}
  \NewEnviron{translation}{}[]
\fi

% This could be redefined to, say, `&` if you wanted to use reledmac/reledpar.
\NewDocumentCommand{\eol}{}{\\}

\endinput
