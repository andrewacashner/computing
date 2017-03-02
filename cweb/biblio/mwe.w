\def\beginlisting{%
	\par\vskip\baselineskip%
	\begingroup%
	\tt\obeylines\obeyspaces%
	\catcode`\{12\catcode`\}12%
}
\def\endlisting{%
	\endgroup\par%
	\vskip\baselineskip%
}

@ Let's say we want to write a \.{CWEB} program that manipulates \.{bibtex} files.
How can we do a multiline verbatim code listing? 

\beginlisting
@@Book{Author:2000,
\  author = {Last,First},
\  title  = {Title},
\  year   = 2000,
}
\endlisting

Thank you, David Carlisle!


