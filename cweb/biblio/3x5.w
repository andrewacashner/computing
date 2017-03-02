% 2015-05-03 -- biblio.w by Andrew A. Cashner

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

@* Introduction.
This is \.{3x5}, a program to build a bibliographic database from input with
minimal markup.
The goal is to allow a quick way to add to a database in \.{biblatex} format,
as simple as writing down the info on a $3 \times 5$ notecard.

Normally a \.{biblatex} entry looks like this:

\beginlisting
@@Book{Kircher:Musurgia,
\  author   = {Kircher, Athanasius},
\  title    = {Musurgia universalis},
\  location = {Rome},
\  year     = 1650,
}
\endlisting

In our system the user specifies in advance which fields will be included for
each entry, like so:

\beginlisting
\# book: au, ti, loc, yr
Kircher, Athanasius
Musurgia universalis
Rome
1650
\endlisting

Each entry is separated by a blank line, just like paragraphs in \TeX.
A line beginning with \.{\#} specifies the contents of the fields for every
subsequent entry. 
\TeX-style comment lines starting with \.{\%} are ignored.

In this way authors can add many entries in a similar format at once, adding
another \.{\#} instruction line if a new setup of fields is needed.
The user may specify a default set of fields by writing a field-instruction line
that begins \.{\#DEFAULT}, like so:

\beginlisting
\#DEFAULT book: au, ti, loc, pub, yr
\endlisting

Then after changing the field instruction with a \.{\#} line, the user may
restore the default by writing a line with only the entry type, omitting the fields:

\beginlisting
\# book
\endlisting

The user may use the exact field names (\.{author}, \.{title}, and so on), or a
default set of abbreviations for the fields (\.{au}, \.{ti}).
The user may also specify custom abbreviations using an \.{\#ABBREV} instruction,
as follows:

\beginlisting
\#ABBREV kw:keyword od:origdate
\endlisting

Each abbreviation is followed by a colon and then the full field name, and each
abbreviation is separated by a space.

% TODO give full example in separate file

@ Macro definitions.

@d MAX_FILENAME 50
@d BUFFER_SIZE  1056
@d MAX_LINE 320

@ Dummy program 

@p
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
@#
int main(int argc, char *argv[])
{
	@<Main variables@>@;
	@<Check arguments from command line@>@;
	@<Open files for reading and writing@>@;
	@<Read one entry@>@;
	@<Close files@>@;
	return(0);
}

@* Process input.

@<Main variables@>=
char infilename[MAX_FILENAME];
char outfilename[MAX_FILENAME];
FILE *infile;
FILE *outfile;

@ User specifies the name of an input file and a destination .bib file.

@<Check arguments...@>=
if (argc != 3) {	
	fprintf(stderr, "Incorrect number of arguments.\n");
	exit(EXIT_FAILURE);
}
printf("Read file %s and add to bib file %s.\n", argv[1], argv[2]);

@ Open the input and output files.

@<Open files...@>=
strcpy(infilename, argv[1]);
strcpy(outfilename, argv[2]); /* .bib must be included explicitly */
infile = fopen(infilename, "r");
if (infile == NULL) {
	fprintf(stderr, "Could not open file %s for reading.\n", infilename);
	exit(EXIT_FAILURE);
}
outfile = fopen(outfilename, "wr");
if (outfile == NULL) {
	fprintf(stderr, "Could not open file %s for writing.\n", outfilename);
	exit(EXIT_FAILURE);
}

@* We read one bibliographic entry into a buffer.

@<Main variables@>=
char bibentry_buffer[BUFFER_SIZE];
char line[MAX_LINE];
int lines_read;
typedef enum { FALSE, TRUE } boolean;
boolean entry_end;

@ Read one entry.

@<Read one entry@>=
bibentry_buffer[0] = '\0';
while (entry_end == FALSE) {
	fgets(line, sizeof(line), infile);
	if (line == NULL || line[0] == '\n') {
		entry_end = TRUE;
	} else {
		strcat(bibentry_buffer, line);
		++lines_read;
	}
}
printf("%d lines read\n", lines_read);
printf("%s\n", bibentry_buffer);

fputs(bibentry_buffer, outfile);
	
	
@ Close files.

@<Close files@>=
fclose(infile);
fclose(outfile);





