/* READBIB (Andrew Cashner, 2014-03-28):
 * Read quasi-markdown bibliography input file into database 
 */

/* Output into xml format? */ 

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX		 0x100000  /* 1 megabyte */
#define NOTAG    0
#define TAGOPEN  1
#define TAGCLOSE 2
#define INTAGS   3

int main (int argc, char *argv[])
{
	FILE *infile;			/* Input file */
	char infilename[100];	/* Input file name */
	char buffer[MAX];		/* Buffer for file input */ 
	int c;					/* File input character as int */
	int status;				/* Location: Unmarked (NOTAG), 
								*		 open markup tag (TAGOPEN), 
								*		 close markup tag (TAGCLOSE), 
								*		 between markup tags (INTAGS) */ 
	char tagopen[25];		/* Beginning markup tag */
	char tagclose[25];		/* End markup tag */
	char *start_ptr;		/* Pointer to position where marked-up text begins 
								* in file input buffer */
	char *end_ptr;			/* Position of last char of marked-up text */
	int i, j;				/* Loop counters */
	char book[500] = "\0";	/* Complete XML book entry with tags */


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

/* COPY FILE TO BUFFER CHAR ARRAY */
	for (i = 0; (c = fgetc(infile)) != EOF; ++i)
		buffer[i] = (char)c;

/* IDENTIFY START OF MARKED-UP TEXT AND SAVE TEXT */

/* > Start tag for biblio entry
 * # Tag for comment line specifying which DB fields will be given
 *   If absent, default pattern is assumed: for initial version, only use
 *   default
 * Every line begins with new line; blank lines are ignored
 */ 
 
 	strcpy(tagopen, "<book>");
	status = NOTAG;

	for (i = 0; buffer[i] != '\0'; ++i) {
		/* SAVE MARKED-UP TEXT */
		if (status == INTAGS) {
			start_ptr = &buffer[i];
			for (j = 0; j < sizeof(book); ++j) {
				book[j] = *start_ptr;
				++start_ptr;
				++i;
			}
			break;
		}
	
		/* SEARCH FOR OPENING TAG */
		for (j = 0; tagopen[j] != '\0'; ++j) {
			if (buffer[i] == tagopen[j]) {
				status = TAGOPEN;
				if (buffer[i] == tagopen[strlen(tagopen) - 1]) {
					status = INTAGS;
					break;
				}
			} else {
				status = NOTAG;
				break;
			}
		++i;
		}
		
	}

/* PRINT SAVED TEXT */
	printf("Book: %s\n", book);
	
/* CLOSE FILE, EXIT */
	fclose(infile);
	return(0);
}

	




		
		
