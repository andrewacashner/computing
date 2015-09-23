/* READBIB (Andrew Cashner, 2014-03-29)
 * 
 * Read bibliography file in quasi-markdown structure;
 * Parse contents and store in them in internal database;
 * Write database contents in bibliography format to external file and stdout.
 *
 * Initial version with fixed memory allocation, limited features.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_ENTRIES 10	/* For now */
#define BIB 0			/* States for parsing */
#define META 1

int main(int argc, char *argv[])
{
	FILE *infile;
	char infilename[100];
	FILE *outfile;
	char outfilename[100];

	FILE *csv_out;						/* .csv output file */
	char csv_name[15] = "books.csv";	/* Filename of .csv */

	int fieldn;				/* Field number in data structure */
	int docn;				/* Counter for number of biblio entries read */
	int state;				/* META (metadata, not a bibliography) or BIB
								(inside a bibiliography) */
	char line[500];			/* Input line from file; fixed size for now */
	int i;					/* Loop counter */
	
	struct bib {
		char au  [100];	/* Author: Last, First */
		char ti  [200];	/* Title */
		char loc [100];	/* Location of publication */
		char pub [100];	/* Publisher */
		char yr  [50];	/* Year (YYYY), allowing for ranges and "circa" */
	};
	struct bib bk[MAX_ENTRIES];	/* Our book database */

/* CHECK ARGUMENTS */
	if (argc != 3) {
		fprintf(stderr, "Incorrect number of arguments.\n");
		fprintf(stderr, "Usage: readbib <input file> <output file>\n");
		exit(8);
	}
	
/* OPEN, CHECK FILES */
	strcpy(infilename, argv[1]);
	infile = fopen(infilename, "r");
	if (infile == NULL) {
		fprintf(stderr, "Could not open file %s for reading.\n", infilename);
		exit(8);
	}
	strcpy(outfilename, argv[2]);
	outfile = fopen(outfilename, "w");
	if (outfile == NULL) {
		fprintf(stderr, "Could not open file %s for writing.\n", outfilename);
		exit(8);
	}
	csv_out = fopen(csv_name, "w");
	if (csv_out == NULL) {
		fprintf(stderr, "Could not open .csv file for writing.\n");
		exit(8);
	}


/* PARSE INPUT FILE, COPY DATA INTO DATABASE */
	fieldn = docn = 0;
	state = META;

	while (fgets(line, sizeof(line), infile) != NULL) {
		/* Trim newline */
		line[strlen(line) - 1] = '\0'; 	
		
		if (state == META) {
			switch (line[0]) {
				/* '=' indicates start of biblio entry */
				case '=':	
					state = BIB;
					break;
				/* All other markup or text outside bib entry is ignored (for now) */
				default: 	
					state = META;
			}
		} else if (state == BIB) {
			switch (fieldn) {
				case 0:
					strcpy(bk[docn].au, line);
					++fieldn;
					break;
				case 1:
					strcpy(bk[docn].ti, line);
					++fieldn;
					break;
				case 2:
					strcpy(bk[docn].loc, line);
					++fieldn;
					break;
				case 3:
					strcpy(bk[docn].pub, line);
					++fieldn;
					break;
				case 4:
					strcpy(bk[docn].yr, line);
					++docn;			/* Add to counter of book entries */
					fieldn = 0;		/* Reset field nr for next book entry */
					state = META; 	/* Leave biblio state */
					break;
				default:
					fieldn = 0;
 					state = META; 
					break;
			}
		}
	}

/* PRINT FORMATTED RESULTS TO FILE, STDOUT */
	for (i = 0; i < docn; ++i) {
		printf("\n%s. %s. %s: %s, %s.\n\n", 
			bk[i].au, bk[i].ti, bk[i].loc, bk[i].pub, bk[i].yr);
		fprintf(outfile, "%s. %s. %s: %s, %s.\n\n", 
			bk[i].au, bk[i].ti, bk[i].loc, bk[i].pub, bk[i].yr);
	}
	printf("%d entries written to file %s.\n", docn, outfilename);

/* SAVE RESULTS AS .CSV */

	for (i = 0; i < docn; ++i)
		fprintf(csv_out, "\"%s\", \"%s\", \"%s\", \"%s\", \"%s\"\n",
			bk[i].au, bk[i].ti, bk[i].loc, bk[i].pub, bk[i].yr);
	
/* CLOSE FILES */
	fclose(infile);
	fclose(outfile);
	fclose(csv_out);

	return(0);
}



 
