% TODO make csnames array into linked list

% latex2html simple converter from latex markup to html markup
% Andrew A. Cashner
% 2015-04-14 begun
% 2015-04-21 working proof-of-concept (or of concept's limits)

% LaTeX logo from eplain.tex
\def\LaTeX{L\kern -.26em \raise .6ex \hbox{\sevenrm A}\kern -.15em \TeX} 

@* Introduction.
This is \.{latex2html}, a rudimentary convertor from \LaTeX\ markup to HTML by
Andrew Cashner, \today.
The program reads a \LaTeX\ file written with a strictly limited subset of
markup commands and converts them to HTML5.

For this prototype version, we ignore all the preamble and focus only on the
contents of the \.{document} environment.  We require that {\it all\/} \LaTeX\
commands are written in the following format: \.{\\csname\{argument\}}.  This
means that in every case we can first find the control-sequence name and then
match it to its argument, so that in the HTML we can put opening and closing
elements around the text of the argument.

We use a |group| counter to keep track of nested groups, and as we read each new
control sequence, we store it in an array |csnames[]| indexed to the group
number.  At the end of each group, it is then easy to fetch the right closing
command for the HTML close tag by calling |csnames[group]|.  The
control-sequence names are actually stored as integers and referenced via macros
to reduce the amount of string comparison.

We read one character at a time, starting in |TEXT| mode.  When we encounter a
backslash, we enter |CSNAME| mode and store each character into a |csnames|
buffer until we reach a left brace ($\lbrace$).  In the case of environments,
these are written \.{\\begin\{environment\}} $\dots$ \.{\\end\{environment\}},
so the control sequence is actually the argument of \.{begin}.  Therefore, if
|csnames| is \.{begin}, then we move on to the next string after the left
brace, and store everything up to the right brace ($\rbrace$) as the
|csnames|.

We convert the command into an HTML tag using a dictionary hash table indexed to
the macros for the control-sequence names.  So \.{emph} becomes \.{em} and
\.{section} becomes \.{h1}.  (In a later stage these substitutions will be
adjustable via a YAML-style configuration file.) In a new file we write the new
HTML start-element tag surrounded by angle braces (e.g., \.{<em>}).

Once we read the control sequence and move past the $\lbrace$, we add one to the
|group| counter and enter |TEXT| mode.  We copy the argument text to the output
file.  For normal commands, when we reach a right ending brace ($\rbrace$) we
decrement the |group| counter.  For environment commands, we look for
\.{\\end\{environment\}} instead of $\rbrace$; in that case we enter |END| mode
and ignore all text until the end brace $\rbrace$. At this point we we insert
the closing tag (e.g., \.{</em>}).

At present there is no way to identify paragraphs for \.{<p>} tags. 
For now we require the \LaTeX\ file to surround each paragraph in either a 
\.{\\begin\{para\}} \dots \.{\\end\{para\}} environment or a \.{\\para\{\}} 
command.

Remember, we are requiring pre-validated \LaTeX\ input syntax, as we do not do
any syntax checking.  We need at least a basic error mechanism to escape if this
system goes wrong.

@p
#include <stdio.h> 
#include <stdlib.h>
#include <string.h>
@#
@<Global variables@>@;
@<Function prototypes@>@;
@#
int main(int argc, char *argv[])
{
	@<Main variables@>@;
	@<Write csname dictionary@>@;
	@<Open files for reading and writing@>@;
	@<Process text@>@;
	return(0);
}

@ Global variables.
@d CSNAME_LENGTH 50
@d FILENAME_LENGTH 200

@* Set up linked lists to store TeX commands, matched to HTML tags.
We use one list as the master dictionary, and we build another as we find
csnames in the document.

@<Main variables@>=
csname_dict_entry doc_csnames[MAX_CSNAMES][CSNAME_LENGTH]; /* Buffer for csnames being read, ordered by group */


@<Global variables@>=
enum {@<csname macro labels@>@;} csname_label;
typedef struct csname_dict_entry *dict_ptr;
typedef struct csname_dict_entry {
	int label;
	char latex_name[CSNAME_LENGTH];
	char html_name[CSNAME_LENGTH];
	dict_ptr *link;
} csname_dict_entry;

@ Now the macro names for |csname_label|.
@<csname macro labels@>=
EMPH, TEXTBF, ENQUOTE, SECTION, SUBSECTION, SUBSUBSECTION, PARAGRAPH, ITEMIZE,
ENUMERATE, ITEM, PARA

@ Function: Add to a dictionary of control-sequences.

@<Function prototypes@>=
dict_ptr new_dict(dict_ptr dict_name);

void new_csname(dict_ptr csname_list,
	int new_label, char new_latex_name[] char new_html_name[]);

@ This function creates a dictionary of control sequence names, mapped to HTML
tags.

@p

dict_ptr new_dict(dict_ptr dict_name)
{
	dict_name = malloc(sizeof(csname_dict_entry));
	dict_name->link = NULL;
	return(dict_ptr);
}

void add_to_dict(dict_ptr csname_list,
	int new_label, char new_latex_name[] char new_html_name[])
{
	dict_ptr new_entry = malloc(sizeof(csname_dict_entry));
	new_entry->label = new_label;
	strcpy(new_entry->latex_name, new_latex_name);
	strcpy(new_entry->html_name, new_html_name);
	new_entry->link = NULL;
	csname_list->link = new_entry;
	return;
}



/* Above will create dictionary, but for csnames list found in file, we should
 * just use label, not write out whole strings */

@ Add entries to csname dictionary.

@<Write csname dictionary@>=
new_csname(csname_dict, EMPH, "emph", "em");
new_csname(csname_dict, TEXTBF, "textbf", "strong");
new_csname(csname_dict, ENQUOTE, "enquote", "q");
new_csname(csname_dict, SECTION, "section", "h1");
new_csname(csname_dict, SUBSECTION, "subsection", "h2");
new_csname(csname_dict, SUBSUBSECTION, "subsubsection", "h3");
new_csname(csname_dict, PARAGRAPH, "paragraph", "h4");
new_csname(csname_dict, ITEMIZE, "itemize", "ul");
new_csname(csname_dict, ENUMERATE, "enumerate", "ol");
new_csname(csname_dict, ITEM, "item", "li");
new_csname(csname_dict, PAR, "par", "p");


@* Set up file input and output.
The user gives the file name to be read as a command-line argument \.{latex2html
file} or \.{latex2html file.tex}.  
If there is no \.{.tex} extension we add it to the filename before opening the
input file and testing for success.
For the output file, if there is a \.{.tex} extension given we strip it;
either way we add the \.{.html} extension for the output filename.


@<Main variables@>=
int group; /* Counter for hierarchical grouping level */
int c; /* Current character as |int| */
FILE *infile;
FILE *outfile;
char infilename[FILENAME_LENGTH];
char outfilename[FILENAME_LENGTH];
int char_position; /* Counter for char array */

@ Open files for reading and writing.

@<Open files...@>=
if (argc != 2) {
	fprintf(stderr, "Usage: latex2html <filename>\n");
	exit(EXIT_FAILURE);
}
strcpy(infilename, argv[1]);
if (strcmp(&infilename[strlen(infilename) - 4], ".tex") != 0) {
	strcat(infilename, ".tex"); /* Add .tex extension if not there */
}
infile = fopen(infilename, "r");
if (infile == NULL) {
	fprintf(stderr, "Unable to open file %s for reading.\n", infilename);
	exit(EXIT_FAILURE);
}
strcpy(outfilename, argv[1]);
if (strcmp(&outfilename[strlen(outfilename) - 4], ".tex") == 0) {
	outfilename[strlen(outfilename) - 4] = '\0';
}
strcat(outfilename, ".html"); /* Add or substitute .html extension */
outfile = fopen(outfilename, "w");
if (outfile == NULL) {
	fprintf(stderr, "Unable to open file %s for writing.\n", outfilename);
	exit(EXIT_FAILURE);
}



@* Process text to convert commands to tags.


@<Global variables@>=
enum { TEXT, CSNAME, BEGIN, END, COMMENT } mode;


@ We build a state machine to read each character and either convert the csnames
to HTML tags or just copy the text directly. 
For \.{\\begin\{environment\}} commands, the csname is taken from the argument
of \.{\\begin\}}.
For normal commands the csname comes from the characters between the backlash
and the start brace, so \.{emph} in \.{\\emph\{word\}}.

@<Process...@>=
mode = TEXT;
group = 0;
char_position = 0;

while ((c = fgetc(infile)) != EOF) {
	switch (mode) {
		@<case TEXT mode@>@;
		@<case CSNAME mode@>@;
		@<case BEGIN environment mode@>@;
		@<case END environment mode@>@;
		@<case COMMENT mode @>@;
	}
}

@ When we are scanning normal text, we copy each character until we encounter a
backslash, which triggers the start of a new csname, or a right brace, which
ends a group.

@<case TEXT...@>=
case TEXT: 
	if (c == '\\') {
		mode = CSNAME;
		char_position = 0;
		++group;
		continue;
	}
	if (endpara == TRUE) {
		endpara = FALSE;
	}
	if (c == '}') {
		mode = TEXT;
		@<Convert and print end tag@>@;
		--group;
		continue;
	}
	if (c == '%') {
		mode = COMMENT;
		continue;
	}
	if (c == '\n') {
		mode = NEWLINE;
	}
	fputc(c, outfile);
	break;

@ When we are inside a csname, we store the characters into |csnames[group]| for
the current group, until we reach the end of the csname at the left brace.

@<case CSNAME...@>=
case CSNAME: 
	if (c == '{') {
		csnames[group][char_position] = '\0';
		@<Check for environment begin or end@>@;
		@<Check for paragraph start@>@;
		@<Convert and print start tag@>@;
		mode = TEXT;
		continue;
	}
	csnames[group][char_position] = (char)c;
	++char_position;
	break;

@ We check to see if the csname is \.{begin} or \.{end} because these cases
require special treatment.

@<Check for environment...@>=
if (strcmp(csnames[group], "begin") == 0) {
	char_position = 0;
	mode = BEGIN;
	continue; 
} 
if (strcmp(csnames[group], "end") == 0) {
	mode = END;
	continue;
}
@ When we have found a \.{\\begin} environment command, we use the csname that
follows in braces instead of \.{begin}.

@<case BEGIN...@>=
case BEGIN:
	if (c == '}') {
		csnames[group][char_position] = '\0';
		@<Convert and print start tag@>@;
		mode = TEXT;
		continue;
	}
	csnames[group][char_position] = (char)c;
	++char_position;
	break;

@ When we have found a \.{\\end} environment command, we do not store any of the
command, but simply treat it as the end of a group. 
(Remember, we are assuming properly formed \LaTeX\ input.)

@<case END...@>=
case END:
	if (c == '}') {
		mode = TEXT;
		--group;
		@<Convert and print end tag@>@;
	}
	continue;
	break;

@ If a comment character \.{'\%'} is found, we omit everything up to the next newline.
@<case COMMENT...@>=
case COMMENT:
	if (c == '\n') {
		mode = TEXT;
	}
	break;

@ Identifying \.{<p>} elements. 
TeX interprets a blank line in the source---two newlines in a row---as a
\.{\\par} command.  
In most cases a \.{\\par} will indicate the end of one paragraph group and the
beginning of another (\.{</p><p>}).
But certain elements should not be put inside a \.{<p>} element, even if they
are preceded by a blank line (such as \.{\\section} which becomes \.{<h>}).
At the first newline we enter |NEWLINE| mode. 
If we encounter another newline, we set the boolean switch |endpara| to |TRUE|
and return to |TEXT| mode.
If the next thing we find is text, we reset the |endpara| switch and insert
\.{<p>} tags; if the next thing we find is a csname, the |endpara| switch will
signal that we need to check a table to determine whether to insert the \.{<p>}
tag.

@<Global variables@>=
typedef struct enum { FALSE, TRUE } boolean;
boolean endpara;

@ After first newline has been found, we are in |NEWLINE| mode.
@<case NEWLINE...@>=
case NEWLINE:
	if (c == '\n') {
		endpara = TRUE;
	}
	mode = TEXT;
	break;

@ In case |CSNAME|, if |endpara| is |TRUE|, we check for the beginning of a
paragraph.

@<Check for paragraph...@>=

if (endpara == TRUE) {
	++group;
	strcpy(csnames[group], "par");
	@<Convert and print end tag@>@;
	--group;
	/* Needs different csnames data structure */
	if (par_csnames[csnames[group].id]) != NOPAR) {
		@<Convert and print start tag@>@;
	}
	@<Convert and print start tag@>@;
}

@* Convert tags.
When we have identified a csname, we convert it to a start HTML tag.

@<Convert and print start tag@>=
csname2html(csnames[group]);
fprintf(outfile, "<%s>", csnames[group]);

@ At the end of each group, we convert the most recent csname to an {\it end} HTMl
tag.

@<Convert and print end tag@>=
csname2html(csnames[group]);
fprintf(outfile, "</%s>", csnames[group]);


@ Function |csname2html|. 

@<Function prototypes@>=
void csname2html(char currentcsname[]);

@ This function converts a TeX csname to an HTML tag according to the rules
setup in our dictionary.
It takes the most recently read csname, |csnames[group]|, as its argument,
and replaces that string with the HTML version.

@p
void csname2html(char currentcsname[])
{
	int i; /* Loop counter */
	for (i = 0; i < MAX_CSNAMES; ++i) {
		if (strcmp(currentcsname, csname_dict[i].latex_name) == 0) {
			strcpy(currentcsname, csname_dict[i].html_name);
		} 
	}
	return;
}

