/* PARSE (Andrew Cashner, 2014-03-14): Practice parsing input file */

/* Read input file, distinguish different types of markup, output to stdout */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TXT 0
#define COMMENT 1

/* FUNCTION to remove comments */
void rm_comments(FILE *input, char start[], char end[]); 

/****************************************/
int main (int argc, char *argv[])
{
	FILE *infile;			/* Input file */
	char infilename[100];	/* Input file name */

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
	rm_comments(infile, "/*", "*/");

/* CLOSE FILE, EXIT */
	fclose(infile);
	return(0);
}

/****************************************
 * FUNCTION rm_comments                 *
 ****************************************/

/* FIX: Find multi-line comments */

void rm_comments(FILE *input, char start[], char end[]) {

	int c, cprev;	/* Current character, previous character read from file */
	int status; 	/* In main text (TXT) or inside comment markup (COMMENT) */

	c = cprev = 0;

	for (status = TXT; ((c = fgetc(input)) != EOF); cprev = c) {
		if (status == TXT) {
			if ((char)c == start[0])
				status = COMMENT;
			else if ((char)cprev == start[0] && (char)c == start[1])
				status = COMMENT;
			else putchar(c); /* If not comment, copy to stdout */

		} else if (status == COMMENT) {
			if ((char)cprev == end[0] && (char)c == end[1])
				status = TXT;
		}
	}
	return;
}
