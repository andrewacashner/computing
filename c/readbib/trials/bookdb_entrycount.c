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
#define EOL 1
#define TXT 0


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
	int c;					/* For copying characters */

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

	while ((c = fgetc(in_file)) != EOF) {

		/* Dummy function: Count number of biblio entries (double \n) */
		if (c == '\n') {
			if (last == EOL)
				++entries;
			else last = EOL;
		} else last = TXT;
	}

/* PRINT DATABASE CONTENTS */

	/* Dummy function: Print number of entries */
	printf("Number of entries in bibliography file: %d\n", entries);

/*
	for (i = 0; i < MAX; ++i) {
		printf("%s %s, *%s* (%s: %s, %s)\n\n", 
			book[i].au_f, book[i].au_l, book[i].ti, book[i].loc,
			book[i].pub, book[i].yr);
	}
*/

	fclose(in_file);
	return (0);
}

