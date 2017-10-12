\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{img2pdf}[2017/10/12
Convert set of images to sing full-page PDF]
\newif\iflscape
\lscapefalse
\DeclareOption{lscape}{\lscapetrue}
\ProcessOptions\relax
\LoadClass{article}

\RequirePackage[margin=1in]{geometry}
\RequirePackage{graphicx}

\iflscape
\RequirePackage{pdflscape}
\fi
\newcommand{\@includeimage}[1]{%
    \noindent\includegraphics[height=\textheight,width=\linewidth,keepaspectratio]{#1}%
}
\newcommand{\includeimage}[1]{%
    \iflscape
    \begin{landscape}
        \@includeimage{#1}
    \end{landscape}
    \else
        \@includeimage{#1}
    \fi
    \clearpage
}
\endinput
