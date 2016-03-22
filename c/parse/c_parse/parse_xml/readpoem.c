/* readpoem.c -- Andrew Cashner -- 2014-08-08
 * 	v2 of readstates.c
 *	Read, store, and print the tags and text of XML file
 */

/* This repeatedly stores each new open or close tag, or text in the same char
 * array, thus it looks like there is empty unmarked up text between <poem> and
 * <stanza>.
 * For the next stage, need to really store all the tags so that they can be
 * compared and a tree built.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define OUT 0
#define IN 1
#define TAG_CLOSE 0
#define TAG_OPEN 1

int main(int argc, char **argv)
{
	int c;
	int state;	/* IN or OUT of markup tag */
	int tag_type;	/* TAG_OPEN or TAG_CLOSE */
	FILE *infile;
	char tag_open[50];
	char tag_close[50];
	char text[200];
	char *tag_open_ptr = tag_open;
	char *tag_close_ptr = tag_close;
	char *text_ptr = text;

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
			if (c == '>')	{
				state = OUT;
				if (tag_type == TAG_OPEN) {
					*tag_open_ptr = '\0';
					fprintf(stdout, "Start: %s\n", tag_open);
				} else if (tag_type == TAG_CLOSE) {
					*tag_close_ptr = '\0';
					fprintf(stdout, "End: %s\n", tag_close);
				}
				text_ptr = text;
				continue;
			} else if (tag_type == TAG_OPEN) {
				*tag_open_ptr = (char)c;
				++tag_open_ptr;
			} else if (tag_type == TAG_CLOSE) {
				*tag_close_ptr = (char)c;
				++tag_close_ptr;
			}
		} else if (state == OUT) {
			if (c == '<') {
				*text_ptr = '\0';
				fprintf(stdout, "Contents: %s\n", text);

				state = IN;
				if ((c = fgetc(infile)) == '/') {
					tag_type = TAG_CLOSE;
					tag_close_ptr = tag_close;
				} else {
					tag_type = TAG_OPEN;
					ungetc(c, infile);
					tag_open_ptr = tag_open;
				}
				continue; /* Go to next char */
			}
			else {
				*text_ptr = (char)c;
				++text_ptr;
			}
		}
	}
	
	/* Store contents in binary tree */

	/* Print contents in specified format */

	return (0);

}
