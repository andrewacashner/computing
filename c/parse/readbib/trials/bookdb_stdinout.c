/* Book database (Andrew Cashner, 2014-03-12) */

/* First stage: Read data from standard input into data structure and print
 * contents to standard output */

#include <stdio.h>
#include <string.h>

#define MAX 3

#define READ_LINE { \
	fgets(line, sizeof(line), stdin); \
	line[strlen(line) - 1] = '\0'; }

#define PRINT_HRULE { printf("=========================================\n\n"); }

int main (void)
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
	struct bib book[MAX];		/* Structure for book data: 
								First stage only holds three */	
	char line[200];			/* Buffer for user input */
	int i;					/* Loop counter */

	printf("Book DB\n");
	PRINT_HRULE

	/* Read data from standard input and store in structure */
	for (i = 0; i < MAX; ++i) {
		
		/* Set book ID number, count from 1 */
		book[i].id = i + 1; 	
		/* Get book data */
		printf("Book %d:\n\n", book[i].id);
		READ_LINE
	    strcpy(book[i].au_l, line);
		READ_LINE
	    strcpy(book[i].au_f, line);
		READ_LINE
		strcpy(book[i].ti, line);
		READ_LINE
		strcpy(book[i].loc, line);
		READ_LINE
		strcpy(book[i].pub, line);
		READ_LINE
		strcpy(book[i].yr, line);
		printf("\n");
	}

	/* Print data to standard output as list */ 
	printf("Database contents:\n");	
	PRINT_HRULE
	
	for (i = 0; i < MAX; ++i) {
		printf("%s %s, *%s* (%s: %s, %s)\n\n", 
			book[i].au_f, book[i].au_l, book[i].ti, book[i].loc,
			book[i].pub, book[i].yr);
	}
	PRINT_HRULE

	return (0);
}

