/* COMPOSER DATABASE */

/* Second stage: Read data from standard input into data structure and write to
 * file */

#include <stdio.h>
#include <stdlib.h>

#define MAX_ENTRIES 2				/* Total number of entries */
#define FILENAME "composerdb.txt"	/* Output file name */

int main()
{

	struct bio {
		char 	last[30];	/* Last name */
		char	first[30];	/* First name */
		int		birth;		/* Year of birth */
		int		death;		/* Year of death */
		};

	struct bio composers[MAX_ENTRIES];	/* Data structure for composer bios */

	char line[100];			/* Buffer for user input */
	int i, n;				/* Loop counters */

	FILE *out_file;						/* Output file */
	char filename[50] = FILENAME;		/* Output file name */

	/* Open file and test for success */
	out_file = fopen(filename, "w");
	
	if ((out_file == NULL)) {
		fprintf(stderr, "\nCould not open file for writing.\n");
		exit(8);
	}

	/* Elicit data input */
	printf("\nComposer Database: Enter data for %d composers.", MAX_ENTRIES);
	
	for (i = 0; i < MAX_ENTRIES; ++i) {
		
		printf("\nLast name: ");

			fgets(line, sizeof(line), stdin);
			for (n = 0; line[n] != '\0'; ++n) {
				composers[i].last[n] = line[n];
			}
			composers[i].last[n - 1] = '\0';
				
		printf("First name: ");

			fgets(line, sizeof(line), stdin);
			for (n = 0; line[n] != '\0'; ++n) {
				composers[i].first[n] = line[n];
			}
			composers[i].first[n - 1] = '\0';
			
		printf("Year of birth (YYYY): ");

			fgets(line, sizeof(line), stdin);
			if (line[0] != '\n')
				sscanf(line, "%d", &composers[i].birth);

		printf("Year of death: ");

			fgets(line, sizeof(line), stdin);
			if (line[0] != '\n')
				sscanf(line, "%d", &composers[i].death);

	} 
	
	/* Print to stdout and to file */

	for (i = 0; i < MAX_ENTRIES; ++i) {
		printf("\n\n%s, %s (%d-%d)", 
			composers[i].last, composers[i].first, 
			composers[i].birth, composers[i].death);
	}
	printf("\n");

	for (i = 0; i < MAX_ENTRIES; ++i) {
		fprintf(out_file, "%s, %s (%d-%d)\n", 
			composers[i].last, composers[i].first, 
			composers[i].birth, composers[i].death);
	}

	fclose(out_file);

	return (0);
}
