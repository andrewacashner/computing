/********************************************
* WRITE -- simple text editor				*
*											*
* 	Takes input from stdin and writes it	*
*	to a text file							*
*											*
*	Parameters: name of output file			*
********************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[])
{
	FILE *out_file;		/* Output file */
	char filename[50];	/* Output filename */	
	int c;				/* Characters input */
	int chars;			/* Number of characters written */

	/* Check for correct number of arguments */
	if (argc != 2) {
		fprintf(stderr, "Incorrect number of arguments.\n");
		fprintf(stderr, "Usage: write <filename>\n");
		exit(8);
	}
	
	/* Get filename, open file, check for error */
	strcpy(filename, argv[1]);

	out_file = fopen(filename, "w");

	if (out_file == NULL) {
		fprintf(stderr, "Could not open output file for writing.\n");
		exit(8);
	}

	/* Print greeting */
	printf("\nWRITE text editor (Ctrl-D to exit)");
	printf("\n==================================\n\n");

	/* Get input from stdin, write to file */
	while ((c = getchar()) != EOF) {
		++chars;
		fputc(c, out_file);
	}
		
	/* Confirm file written */
	printf("\n\n==================================");
	printf("\n%d characters written to file %s\n\n", chars, filename);

	/* Close file */
	fclose(out_file);

	return (0);
}



	

