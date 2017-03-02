/* mdindex.c -- Make subject index from markdown files with keyword
 * lists 
 * Andrew Cashner, 2016/11/4
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_FILENAME 100
#define MAX_LINE 1200

int main(int argc, char *argv[])
{
	FILE *infile, *outfile;
	char infilename[MAX_FILENAME], outfilename[MAX_FILENAME];
	char line[MAX_LINE], test_str[MAX_LINE];
	bool found = false;
	
	if (argc != 3) {
		fprintf(stderr, "Incorrect arguments. Usage: mdindex <input_file> <output_file>\n");
		exit(EXIT_FAILURE);
	}

	strcpy(infilename, argv[1]);
	infile = fopen(infilename, "r");
	if (infile == NULL) {
		fprintf(stderr, "Could not open file %s for reading.\n", infilename);
		exit(EXIT_FAILURE);
	}
	
	strcpy(outfilename, argv[2]);
	outfile = fopen(outfilename, "w");
	if (outfile == NULL) {
		fprintf(stderr, "Could not open file %s for writing.\n", outfilename);
		exit(EXIT_FAILURE);
	}

	if (strcmp(&infilename[strlen(infilename) - 3], ".md") == 0) {
		sprintf(&infilename[strlen(infilename) - 2], "html");
	}

	fprintf(outfile, "# SUBJECT INDEX\n\n");
	
	while (fgets(line, sizeof(line), infile) != NULL) {
		if (found == true) {
			if (line[0] == '#') {
				break;
			}
			if (line[0] != '\n') {
				line[strlen(line) - 1] = '\0';
				fprintf(outfile, "* %s [1](%s)\n", line, infilename);
			}
		} else if (line[0] == '#' && line[1] == ' ') {
			sscanf(&line[2], "%s", test_str);
			if (strcmp(test_str, "Keywords") == 0) {
				found = true;
				continue;
			} else {
				found = false;
			}
		}
	}
	
	
	fclose(infile);
	fclose(outfile);
	return(0);
}

