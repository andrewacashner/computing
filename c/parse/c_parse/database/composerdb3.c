/* COMPOSER DATABASE */

/* Third stage: Read data from single file into data structure and write to
 * new file.
 * Input file must have one data element per line */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[])
{

	struct bio {
		char 	last[30];	/* Last name */
		char	first[30];	/* First name */
		int		birth;		/* Year of birth */
		int		death;		/* Year of death */
		};
	struct bio composers;	/* Data structure for single composer bio */
	char line[100];			/* Buffer for user input */
	int  n;					/* Loop counter */
	FILE *in_file;			/* Input file */
	FILE *out_file;			/* Output file */
	char infilename[50];	/* Output file name */
	char outfilename[50];	/* Output file name */
	
	/* Check number of arguments */
	if (argc != 3) {
		fprintf(stderr, "\nIncorrect number of arguments.");
		fprintf(stderr, "\nUsage: composerdb <inputfile> <outputfile>\n");
		exit(8);
	}

	/* Get file names */
	strcpy(infilename, argv[1]);
	strcpy(outfilename, argv[2]);

	/* Open input and files, test for success */
	in_file = fopen(infilename, "r");
	out_file = fopen(outfilename, "w");

	if ((in_file == NULL)) {
		fprintf(stderr, "\nCould not open input file for reading.\n");
		exit(8);
	}
	if ((out_file == NULL)) {
		fprintf(stderr, "\nCould not open output file for writing.\n");
		exit(8);
	}

	/* Read input file data into data structure, 1 line per element */

	/* Last name */
	fgets(line, sizeof(line), in_file);
	for (n = 0; line[n] != '\0'; ++n) {
		composers.last[n] = line[n];
	}
	composers.last[n - 1] = '\0';
				
	/* First name */
	fgets(line, sizeof(line), in_file);
	for (n = 0; line[n] != '\0'; ++n) {
		composers.first[n] = line[n];
	}
	composers.first[n - 1] = '\0';
		
	/* Birth year */
	fgets(line, sizeof(line), in_file);
	sscanf(line, "%d", &composers.birth);

	/* Death year */
	fgets(line, sizeof(line), in_file);
	sscanf(line, "%d", &composers.death);

	
	/* Print data to stdout in new format */

	printf("\n\n%s, %s (%d-%d)", 
			composers.last, composers.first, 
			composers.birth, composers.death);
	printf("\n\n");

	/* Print data to output file in new format */

	fprintf(out_file, "%s, %s (%d-%d)\n", 
		composers.last, composers.first, 
		composers.birth, composers.death);

	/* Close files and exit */
	fclose(in_file);
	fclose(out_file);

	return (0);
}
