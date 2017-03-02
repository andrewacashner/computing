/* readpoem.c -- Andrew Cashner -- 2014-08-011 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define OUT 0
#define IN 1
#define TAG_OPEN 2
#define TAG_CLOSE 3

int main(int argc, char **argv)
{
	int c;
	int state;	/* IN or OUT of markup tag */
	int tag_type;	/* TAG_OPEN or TAG_CLOSE */
	FILE *infile;

	/* Read command line argument to get input filename 
	 * (later, also output format) */
	if (argc != 2) {
		fprintf(stderr, "Incorrect number of arguments.\n");
		fprintf(stderr, "Usage: readpoem <input_file_name>\n");
		exit(8);
	}
	infile = fopen(argv[1], "r");
	if (infile == NULL) {
		fprintf(stderr, "Could not open file %s for reading.\n", argv[1]);
		exit(8);
	}
	
	/* Read input file, parse for structure */
	tag_type = TAG_OPEN;
	state = OUT;

	while ((c = fgetc(infile)) != EOF) {
		if (state == IN) {
			switch (c) {
				case ('/'):
					tag_type = TAG_CLOSE;
					continue;
				case ('>'):
					state = OUT;
					if (tag_type == TAG_CLOSE) {
						tag_type = TAG_OPEN;
					}
					else fputc('\n', stdout);
					continue;
				default:
					if (tag_type == TAG_OPEN)
						fputc(c, stdout);
			}
		} else if (c == '<') 
			state = IN;
		else state = OUT;
	}

	
	/* Store contents in binary tree */

	/* Print contents in specified format */

	return (0);

}
