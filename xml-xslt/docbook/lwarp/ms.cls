\NeedsTeXFormat{LaTeX2e}
\ProvidesPackage{ms}[2021/09/21 academic manuscript]
\LoadClass{article}

\RequirePackage{lwarp}
\booltrue{FormatWP}

\RequirePackage{graphicx}
\RequirePackage{semantic-markup}

\RequirePackage{newfloat}
\DeclareFloatingEnvironment[
    name =      {Example},
    listname =  {List of Examples},
    fileext =   {ex},
    placement = {ht!}
]{example}

\endinput
