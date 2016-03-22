\let\ifpdf\undefined
\input eplain
\pdffalse

@* Minimal working example.
This is a \.{CWEB} program that uses \.{eplain} macros.

\numberedlist
\li First
\li Second
\endnumberedlist

@p
#include <stdio.h>

int main(void)
{
	@<Say hello@>@;
	return(0);
}

@ Here is the message we will print.

@<Say hello@>=
printf("Hello, TeX!\n");
