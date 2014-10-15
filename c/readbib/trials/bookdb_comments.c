/* Book database (Andrew Cashner, 2014-03-12) */

/* DONE--First stage: Read data from standard input into data structure and print
 * contents to standard output
 *
 * Second stage: Read and parse data from file, store in data structure, print
 * contents to standard output 
 *
 *
 * File entries will be formatted like this, separated with blank line:
 * 
 *  Last, First
 *  Title
 *  Place: Publisher, Date
 *
 ***********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX 3
#define TXT 0
#define EOL 1
#define COMMENT 2

int main (int argc, char *argv[])
{
	struct bib {
		int id;				/* Unique record number */
		char au_l[100];		/* Author last name */
		char au_f[100];		/* Author first name */
		char ti[200];		/* Title */
		char loc[100];		/* Location of publication */
		char pub[100];		/* Publisher */
		char yr[10];		/* Year of publication, YYYY */
	};
/*	struct bib book[MAX];*/	/* Array of structure for book data */

	FILE *in_file;			/* Input file */
	char in_filename[100];	/* Name of input file */

/*	int i;	*/				/* Loop counter */
	int c;					/* Current character read */
	int cprev; 				/* Previous character read */

	int state = 0;			/* What type of string is being read */
	int last = 0;			/* Value of last character read from input file, EOL or TXT */
	int entries = 1;		/* Number of entries in bibliography file */

/* SETUP */
	/* Check number of arguments */
	if (argc != 2) {
		fprintf(stderr, "Incorrect number of arguments.\n");
		fprintf(stderr, "Usage: bookdb <name of input file>\n");
		exit(8);
	}
	
	/* Get filename, open input file for reading, check for error */
	strcpy(in_filename, argv[1]); 
	in_file = fopen(in_filename, "r");
	if (in_file == NULL) {
		fprintf(stderr, "Could not open file %s for reading.\n", in_filename);
		exit(8);
	}


/* READ FILE CONTENTS INTO DATA STRUCTURE */

	/* Track whether in text or comment; test to see if character signals begin
	 * or end of comment; store previous character to be able to two-char
	 * comment symbol */ 
	for (state = TXT; (c = fgetc(in_file)) != EOF; cprev = c) {

		/* Disregard C-style comments */
		if (state == TXT) {
			if (c == '/')
				state = COMMENT;
			else if ((cprev == '/') && (c == '*'))
				state = COMMENT;
			/* If not inside comment, then copy character */
			else putchar(c); 	
		
		/* If in comment, test for end of comment */
		} else if (state == COMMENT) {	
			if ((cprev == '*') && (c == '/'))
				state = TXT;
		} 

		/* Read database fields from # comments (such as au,ti)  */
		/* Identify start and end of each bib entry */
		/* Read data for those fields from the lines of each entry into struct */
	}

/* PRINT DATABASE CONTENTS */
	/* Not yet its own section */

/* CLEAN UP */
	fclose(in_file);
	return (0);
}

