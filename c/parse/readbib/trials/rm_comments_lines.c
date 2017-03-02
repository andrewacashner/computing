/* PARSE (Andrew Cashner, 2014-03-14): Practice parsing input file */

/* Read input file, distinguish different types of markup, output to stdout */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TXT 0
#define COMMENT 1

void rm_comments(char[], char[], char[]);		/* FUNCTION rm_comments */

int main (int argc, char *argv[])
{
	FILE *infile;			/* Input file */
	char infilename[100];	/* Input file name */
	char fline[500];		/* Line read from file input */
	char markup_start[2];	/* Multicharacter markup code for start of group */
	char markup_end[2];		/* Markup for end of group */

/* CHECK ARGUMENTS */
	if (argc != 2) {
		fprintf(stderr, "Incorrect number of arguments.\n");
		fprintf(stderr, "Usage: parse <input file name>\n");
		exit(8);
	}

/* OPEN FILE */
	strcpy(infilename, argv[1]);
	infile = fopen(infilename, "r");
	if (infile == NULL) {
		fprintf(stderr, "Could not open file %s for reading.\n", infilename);
		exit(8);
	}

/* REMOVE COMMENTS, COPY REST OF FILE TO STDOUT */
	strcpy(markup_start, "/*");
	strcpy(markup_end, "*/");
	
	while (fgets(fline, sizeof(fline), infile) != NULL)
		rm_comments(fline, markup_start, markup_end);

/* CLOSE FILE, EXIT */
	fclose(infile);
	return(0);
}

/****************************************
 * FUNCTION rm_comments                 *
 ****************************************/

/* FIX: Find multi-line comments */

void rm_comments(char line[], char start[], char end[]) {

	int i;
	int status; /* In main text (TXT) or inside comment markup (COMMENT) */

	status = TXT;
	for (i = 0; line[i] != '\0'; ++i) {
		if (status == TXT) {
			if (line[i] == start[0])
				status = COMMENT;
			else if (line[i - 1] == start[0] && line[i] == start[1])
				status = COMMENT;
			else putchar(line[i]); /* If not comment, copy to stdout */

		} else if (status == COMMENT) {
			if (line[i - 1] == end[0] && line[i] == end[1])
				status = TXT;
		}
	}
	return;
}
