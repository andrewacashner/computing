/* PARSE (Andrew Cashner, 2014-03-14): Practice parsing input file */

/* Read input file, distinguish different types of markup, output to stdout */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TXT 0
#define COMMENT 1

/* FUNCTION to remove comments */
void rm_comments(FILE *input, char start[], char end[]); 

/****************************************/
int main (int argc, char *argv[])
{
	FILE *infile;			/* Input file */
	char infilename[100];	/* Input file name */

/* CHECK ARGUMENTS */
	if (argc != 2) {
		fprintf(stderr, "Incorrect number of arguments.\n");
		fprintf(stderr, "Usage: parse <input file name>\n");
		exit(8);
	}

/* OPEN FILE */
	strcpy(infilename, argv[1]);
	infile = fopen(infilename, "r");
	if (infile == NULL) {
		fprintf(stderr, "Could not open file %s for reading.\n", infilename);
		exit(8);
	}

/* REMOVE COMMENTS, COPY REST OF FILE TO STDOUT */
	rm_comments(infile, "/*", "*/");

/* PARSE HTML-STYLE TAGS: <tag> </tag> */
	int c;
	#define TAG_PUNCT 1 /* Tag punctuation */
	#define TAG_OPEN  2 /* Inside opening tag brackets */
	#define TAG_CLOSE 3 /* Inside closing tag brackets */
	#define TXT_MKD   4 /* Text between markup tags*/
	#define TXT_UNMKD 5 /* Non-marked-up text */
	
	status = TXT_UNMKD;

	while ((c = fgetc(infile)) != EOF) {
		if (status == TXT_UNMKD) {
			if (c == '<')
				status = TAG_PUNCT;
		} else if (status == TAG_PUNCT) {
			if (c == '/')
				status = TAG_CLOSE;
			else
				; /* Copy tag contents to char array */

	while (fgets(line, sizeof(line), infile) != NULL) {
		if (status == TXT_UNMKD) {
			if ((test = sscanf(line, "<%s>", &string)) == 0) {
			if (string[0] = '/') {
				status = TAG_CLOSE;
				string_ptr = string[1];
				strcpy(tag_close, *string_ptr);
			}
			else {
				status = TAG_OPEN;
				strcpy(tag_open, string);
			}
/* XX abandon */

/* CLOSE FILE, EXIT */
	fclose(infile);
	return(0);
}

/****************************************
 * FUNCTION rm_comments                 *
 ****************************************/

void rm_comments(FILE *input, char start[], char end[]) {

	int c, cprev;	/* Current, previous character read from file */
	int status; 	/* In main text (TXT) or inside markup (COMMENT) */

	c = cprev = 0;

	for (status = TXT; ((c = fgetc(input)) != EOF); cprev = c) {
		if (status == TXT) {
			if ((char)c == start[0])
				status = COMMENT;
			else if ((char)cprev == start[0] && (char)c == start[1])
				status = COMMENT;
			else putchar(c); /* If not comment, copy to stdout */

		} else if (status == COMMENT) {
			if ((char)cprev == end[0] && (char)c == end[1])
				status = TXT;
		}
	}
	return;
}

