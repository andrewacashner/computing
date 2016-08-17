% Custom document class for MUHL 570 course 
% at University of Southern California
% By Andrew Cashner, Fall 2016

% For this course, do not change this file.
% Must compile with xelatex or lualatex

% TECHNICAL SETUP
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{usc-muhl-article}[2016/08/10 For MUHL 570, USC Fall 2016]
\LoadClass[12pt,titlepage]{article}

% FONTS
% Set up to use any system font.
\RequirePackage{fontspec}
% By default, use Palatino.
\RequirePackage{newpxmath,newpxtext}

% PAGE LAYOUT
% Set 1-inch margins on all sides.
\RequirePackage[margin=1in]{geometry}
% Set double spacing.
\RequirePackage{setspace}
\doublespacing
% Use only one space between sentences.
\frenchspacing

% BIBLIOGRAPHY
% Process and format bibliographic citations from
% a .bib file according to Chicago Manual of Style
\RequirePackage[notes]{biblatex-chicago}
% Start bibliography on new page, make single spaced
\let\oldprintbibliography\printbibliography
\renewcommand{\printbibliography}{%
  \clearpage
  {\singlespacing\oldprintbibliography[title=Bibliography]}%
}

% MARKUP COMMANDS
% Use semantic markup like \term and musical symbols 
% Also provides {Footnote} environment
\RequirePackage{semantics}

\endinput