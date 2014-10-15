/* INSERT: insert one file into an existing file at a   *
 * specified location 						 			*
 * 2014-02-26											*
 *														*
 * Usage: 												*
 * insert <file to insert> <file to insert into> 		*
 * 			<line number>								*
 ********************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int confirm (void);				/* confirm function */

int main (int argc, char *argv[]){

	FILE *insert_file;			/* File to insert = <file 1> */ 
	char insert_name[40];			
	FILE *main_file;				/* File to insert into = <file 2> */
	char main_name[40];			
	FILE *out_file;				/* Output file with combined data */
	char out_name[40];			

	char line_nr_string[20];	/* Line number as char string */
	int  line_nr;				/* Line nr as int; insert pt in <file 2> */

	char file_line[500];		/* Buffer for each line of file */
	int n;						/* Loop counter */

	/* Check for correct number of arguments (4 = command + parameters) */
	if (argc != 4) {
		fprintf(stderr, "Incorrect number of arguments.\n");
		fprintf(stderr, "Usage: insert <file 1> <file 2> <line nr>\n");
		fprintf(stderr, "<file 1> = file to insert\n");
		fprintf(stderr, "<file 2> = file to insert into\n");
		fprintf(stderr, 
			"<line nr> = line number in <file 2> for insert point.\n");
		exit(8);
	}

	/* Get filenames from command-line arguments */
	strcpy(insert_name, argv[1]);
	strcpy(main_name, argv[2]);

	/* Get line number from CLI argument */
	strcpy(line_nr_string, argv[3]);
	sscanf(line_nr_string, "%d", &line_nr);

	/* Create file name for output file */
	strcpy(out_name, "insert_output.txt");

	/* Open file 1 for reading and file 2 for reading and writing */
	/* Check for error */
	insert_file = fopen(insert_name, "r");

	if (insert_file == NULL) {
		fprintf(stderr, "Could not open file %s for reading.\n", insert_name);
		exit(8);
	}

	main_file = fopen(main_name, "r");

	if (main_file == NULL) {
		fprintf(stderr, "Could not open file %s for reading.\n", main_name);
		exit(8);
	}

	out_file = fopen(out_name, "w");

	if (out_file == NULL) {
		fprintf(stderr, "Could not open file %s for writing.\n", out_name);
		exit(8);
	}

	/* Confirm */
	printf("Insert file %s into file %s before line %d.\n",
		insert_name, main_name, line_nr);
	printf("Output filename: %s\n", out_name);

	if (confirm() != 0) {
		fclose(insert_file);
		fclose(main_file);
		fclose(out_file);
		exit(8);
	}

/* Insert file */

	/* Copy main_file to out_file up to line number */
		/* We are assuming that main_file has at least line_nr lines */
	for (n = 0; n < line_nr; ++n) {
		fgets(file_line, sizeof(file_line), main_file);
		fputs(file_line, out_file);
	}


	/* At line number, switch: copy insert_file to out_file (complete) */
	while (fgets(file_line, sizeof(file_line), insert_file) != NULL)
		fputs(file_line, out_file);
	
	/* When end of insert_file is reached, copy rest of main_file to
	 * out_file */
		/* First read the lines up to line_nr but don't copy them */
	for (n = 0; n < line_nr; ++n) {
		fgets(file_line, sizeof(file_line), main_file);
	}
		/* Copy the rest */
	while (fgets(file_line, sizeof(file_line), main_file) != NULL)
		fputs(file_line, out_file);

	/* Close files */
	fclose(insert_file);
	fclose(main_file);
	fclose(out_file);

	return (0);
}

/************************************************************/

/* CONFIRM function */
int confirm (void) {

	char input[100];	/* Buffer for user input */
	int  result;		/* Boolean: 0 yes, 1 no */

	while (1) {
		printf("y/n ? ");
		fgets(input, sizeof(input), stdin);

		if (input[0] == 'y') {
			/* No news is good news in unix */ 
			result = 0;
			break;

		} else if (input[0] == 'n') {
			printf("Command canceled.\n");
			result = 1;
			break;

		} else if (((input[0] != 'y') && (input[0] != 'n')) 
				|| (input[1] != '\n')) {

			printf("Enter y or n.\n");
		}
	}
	return (result);
}
