/* unhtml.c -- practice stripping unwanted portions of HTML file
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char* argv[])
{
	FILE *infile;
	FILE *outfile;

	if (argc > 3) {
		fprintf(stderr, "Too many arguments.\n");
		fprintf(stderr, "Usage: unhtml <inputfile> <outputfile>\n");
		exit(1);
	}

	infile = fopen(argv[1], "r");
	if (infile == NULL) {
		fprintf(stderr, 
			"Could not open file %s for reading.\n", argv[1]);
		exit(1);
	}
	outfile = fopen(argv[2], "w");
	if (outfile == NULL) {
		fprintf(stderr,	
			"Could not open file %s for writing.\n", argv[2]);
		exit(1);
	}

	findtag("main", infile);

	return(0);
}

char *findtag(char query[], FILE *file)
{
	int this_c, prev_c;
	int start_token = "<"
	int end_token = ">"
	char *start;
	enum { OUT, IN } state = OUT;

	c = fgetc(file);
	switch (state) {
		case OUT:
			if (c = start_token)
				state = IN;
		case IN:
			if (c = end_token)

		default:
			; /* do nothing */
	}

	OUT
		this_c <	--> IN
		store
	IN
		this_c >  	--> OUT
		prev_c <
			this_c /	--> END
			else 		--> START
				both cases: store tag contents
	

