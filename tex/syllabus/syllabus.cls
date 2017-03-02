\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{syllabus}[2015/07/15 Syllabus class for university courses]
\LoadClassWithOptions{article}

\RequirePackage{xparse}
\RequirePackage{url}

%************************************************************************
%% SECTION HEADINGS
\newcommand{\Divider}{%
	\noindent\rule{\textwidth}{0.4pt}\par\bigskip%
}

%************************************************************************
%% COURSE INFO 
\newcommand{\setCourseTitle}		[1]{\gdef\@CourseTitle{#1}}
\newcommand{\setCourseShortTitle}	[1]{\gdef\@CourseShortTitle{#1}}
\newcommand{\setCourseRoom}			[1]{\gdef\@CourseRoom{#1}}
\newcommand{\setCourseTime}			[1]{\gdef\@CourseTime{#1}}
\newcommand{\setCourseTerm}			[1]{\gdef\@CourseTerm{#1}}
\newcommand{\setCourseNumber}		[1]{\gdef\@CourseNumber{#1}}
\newcommand{\setCourseInstructor}	[1]{\gdef\@CourseInstructor{#1}}
\newcommand{\setCourseInstitution}	[1]{\gdef\@CourseInstitution{#1}}

\newcommand{\printCourseInfo}{%
	{%
		\setlength{\parindent}{0pt}
		{\Large\bfseries \@CourseTitle}\par
		\vspace{1ex}
		{\large\bfseries \@CourseNumber}\par
		\vspace{2ex}
		\@CourseInstitution, \@CourseTerm\par
		\@CourseRoom, \@CourseTime\par%
	}%
}

\newenvironment{CourseInfo}
	{}
	{\printCourseInfo}

%************************************************************************
%% INSTRUCTOR CONTACT INFORMATION
%% Each PersonID environment creates a set of macros specific to the person named in that environment.
%% It increments a counter that will be used as a unique PersonID number 
%% and used in a numerically ordered array.
\newcounter{personnum}
\newenvironment{PersonID}
	{}
	{\stepcounter{personnum}}

%% #1 \field number #2 text for that field
\newcommand{\setIDelement}[2]{%
	% numeric baseword, e.g., \person0field1
	\def\IDnumericbase{person\thepersonnum field#1}
	\expandafter\gdef\csname\IDnumericbase\endcsname{#2}%
}

%% This command calls up an ID from its numeric macro.
\newcommand{\getIDelement}[2]{%
	\csname person#1field#2\endcsname%
}

%% These commands create ID elements specific to each PersonID environment.
\newcommand{\Role}[1]	{\setIDelement{0}{#1}}
\newcommand{\Name}[1]	{\setIDelement{1}{#1}}
\newcommand{\Office}[1]	{\setIDelement{2}{#1}}
\newcommand{\Phone}[1]	{\setIDelement{3}{#1}}
\newcommand{\Email}[1]	{\setIDelement{4}{\url{#1}}}

%% Macros to refer to the field numbers by name.
\newcommand{\@rolefield}	{0}
\newcommand{\@namefield}	{1}
\newcommand{\@officefield}	{2}
\newcommand{\@phonefield}	{3}
\newcommand{\@emailfield}	{4}

%% List data for a single person in tabular format.
\newcommand{\printPerson}[1]{%
	\def\@person{#1}%
	\parbox[t]{0.4\textwidth}{%
			\setlength{\parindent}{0pt}
			\textbf{\getIDelement{\@person}{\@rolefield}}\par
			\textbf{\getIDelement{\@person}{\@namefield}}\par 
			\getIDelement{\@person}{\@officefield}, 		
				\getIDelement{\@person}{\@phonefield}\par
			\getIDelement{\@person}{\@emailfield}\par
			\vspace{\baselineskip}%
	}\ %
}

%% Print a formatted list of all personnel in tabular format.
\newcounter{thisperson}
\newcommand{\printPersonList}{%
	\setcounter{thisperson}{0}%
	\loop
		\printPerson{\thethisperson}%
		\stepcounter{thisperson}%
		\ifnum\value{thisperson} < \value{personnum}%
		\hfill
	\repeat%
}

%% The Instructors prints the personnel list at the end.
\newenvironment{Instructors}
	{}
	{\ListPeople}


%************************************************************************
%% GRADE SCALE

%% Set the percentage ranges for a letter grade (in csname array)
%% #1 Letter name, #2-4 Bottom value of minus, pure, and plus ranges
\newcommand{\setGradeRange}[4]{%
	\expandafter\gdef\csname GradeRange#1\endcsname{#1 & #2 & #3 & #4}%
}
%% Recall array values
%% #1 Letter name
\newcommand{\@GetGradeRange}[1]{%
	\csname GradeRange#1\endcsname%
}
%% Print table of percentage values
\newcommand{\printGradeScale}{%
	\begin{center}
	\begin{tabular}{lrrr}
		& \multicolumn{1}{c}{$-$} &  & \multicolumn{1}{c}{$+$}\\[1ex]
	\@GetGradeRange{A}\\
	\@GetGradeRange{B}\\
	\@GetGradeRange{C}\\
	\@GetGradeRange{D}\\
	\@GetGradeRange{F}\\ 
	\end{tabular}
	\end{center}%
}
%% Wrap in environment and automatically print grade scale at end.
\newenvironment{GradeScale}
	{}
	{\medskip\printGradeScale}


%************************************************************************
%% SCHEDULE OF CLASSES AND ASSIGNMENTS

\newcounter{week}
\newcommand{\Week}[1]{%
	\stepcounter{week}%
	\subsection*{Week \theweek: #1}%
}
\newcounter{class}
\DeclareDocumentCommand{\Class}{ o m }{%
	\stepcounter{class}%
	\IfNoValueTF{#1}{\def\classday{\relax}}
		{\def\classday{\ (#1)}}
	\subsubsection*{Class \theclass \classday: #2}
}
%************************************************************************
%% Create list of assignments of a particular kind (e.g., 'Reading')

%% Print a list header before the list of assignments;
%% can be a macro (see \ReadingList below) or just plain text.
\newenvironment{Asst}[1]
	{%
		\par\medskip\noindent#1\par
		\begin{enumerate}%
	}
	{\end{enumerate}}

%% Note in parentheses before individual assignment listing
%% E.g., for primary (srcI) vs secondary (srcII) sources
\newcommand{\PreNote}[1]{%
	\makebox[1.5em][l]{(#1)}%
}
\newcommand{\srcI}{\PreNote{i}}
\newcommand{\srcII}{\PreNote{ii}}

%% Examples of custom assignment types
\newcommand{\ReadingList}{Reading}
\newcommand{\MusicList}{Music Listening}

\newenvironment{Reading}
	{\begin{Asst}{\ReadingList}}
	{\end{Asst}}
\newenvironment{Music}
	{\begin{Asst}{\MusicList}}
	{\end{Asst}}
	
\endinput
