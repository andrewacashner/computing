\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{happyholidays}[2016/12/19 Stackexchange holiday challenge]
\LoadClass{article}

\RequirePackage[default]{frcursive}
\RequirePackage[T1]{fontenc}
\RequirePackage{xcolor}
\RequirePackage{xparse}

\NewDocumentCommand{\caroltitle}{ m }{%
  \begin{center}
    \Large\itshape\bfseries #1
  \end{center}\par
  \addvspace{1em}%
}

\NewDocumentEnvironment{carol}{}
{\begin{center}\color{red}
 \begin{tabular}{l}}
{\end{tabular}
 \end{center}}

\NewDocumentCommand{\eol}{}{\tabularnewline}

\NewDocumentCommand{\Falala}{ m }{%
  \textcolor{green}{Fa la la la la, la la la la#1}%
}

\NewDocumentCommand{\illustration}{}{%
  \rule{\textwidth}{1in}\par
}


\endinput
