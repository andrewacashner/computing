/* interval_calc_args.c -- Andrew Cashner, 2015-05-15
 * Test arguments for interval_calc program
 * 1. Read line from stdin
 * 2. Parse into words (should be three)
 * 3. Check each word
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_ACCIDENTALS 4

struct music_sign {
	char symbol;
	int value;
};
struct music_sign accidental_table[MAX_ACCIDENTALS];

int main(void)
{
	int i;
	enum { TRUE, FALSE } found;
	char input_line[100];
	char *line_ptr;
	int pitch_class;
	int accidental;
	int octave;
	int operator;

	accidental_table[0].symbol = 'B';
	accidental_table[1].symbol = 'b';
	accidental_table[2].symbol = 'n';
	accidental_table[3].symbol = '#';
	accidental_table[4].symbol = 'X';
	accidental_table[0].value = -2;
	accidental_table[1].value = -1;
	accidental_table[2].value =  0;
	accidental_table[3].value =  1;
	accidental_table[4].value =  2;


	while (1) {	

	/* Get input line from standard input */
	fgets(input_line, sizeof(input_line), stdin);
	printf("%s", input_line);
	input_line[strlen(input_line) - 1] = '\0';

	/* Check for quit command */	
	if (input_line[0] == 'q' && input_line[1] == '\0') {
/* DEBUG */	printf("You entered the quit command.\n");
		return(0);
	}

	/* Parse into three words at space divisions */
	int num_words;
	int num_chars;
	char octave_word[5];
	char operator_word[2];
	char interval_word[4];
	enum { _OUT, _IN } parse_state;

	line_ptr = input_line;
	parse_state = _OUT;
	for (i = 0; input_line[i] != '\0'; ++i) {
		if (parse_state == _OUT) {
			if (line_ptr[0] == ' ') {
				++line_ptr;
			} else {
				parse_state = _IN;
				++num_chars;	
				++num_words;
			}
		if (parse_state == _IN) {
			if (line_ptr[0]
			 /* START HERE XXX */

	/* Check first char: should be A - G */
	if (input_line[0] < 'A' || input_line[0] > 'G') {
		fprintf(stderr, "Bad input '%c' at position 0\n", 	
			input_line[0]);
		exit(EXIT_FAILURE);
	}
	/* Convert to int; adjust for A and B since C = 0 */
	pitch_class = (int)(input_line[0] - 'C');
	if (pitch_class < 0) {
		pitch_class += 7;
	}
/* DEBUG */	printf("pitch_class %d\n", pitch_class);

	/* Accidental can be omitted if it is natural */
	if (input_line[1] >= '0' && input_line[1] <= '9') {
		accidental = 0;
		line_ptr = &input_line[1];
/* DEBUG */	printf("No accidental entered; natural assumed.\n");
	} else {
		/* Check second char in table of accidentals */
		for (i = 0, found = FALSE; i < MAX_ACCIDENTALS; ++i) { 
			if (input_line[1] == accidental_table[i].symbol) {
				accidental = accidental_table[i].value;
				line_ptr = &input_line[2];
				found = TRUE;
			}
		}
		if (found == FALSE) {
			fprintf(stderr, "Bad input '%c' at position 1\n",
				input_line[1]);
			exit(EXIT_FAILURE);
		}
	}
/* DEBUG */	printf("accidental %d\n", accidental);

/* DEBUG */	printf("*line_ptr %c\n", *line_ptr);

	/* Check to see that octave number is 0-99, two digits max */
	if (strlen(line_ptr) > 2) {
		fprintf(stderr, "Only two digits 0-99 allowed for "
			"octave.\n");
		exit(EXIT_FAILURE);
	}
	if (strlen(line_ptr) == 2)  {
		if (*(line_ptr + 1) < '0' || *(line_ptr + 1) > '9') {
			fprintf(stderr, "Bad octave number\n");
			exit(EXIT_FAILURE);
		}
	}	
	if (*line_ptr >= '0' && *line_ptr <= '9') {
	     	sscanf(line_ptr, "%d", &octave);
	}
/* DEBUG */ printf("octave %d\n", octave);

	/***************************************************************/
	/* OPERATOR */
/* This does not work because previous code assumes an end-of-string after
 * octave number, while this treats it as one continuous string */
	/* Skip any spaces 
	++line_ptr;
	while (line_ptr[0] == ' ') {
		++line_ptr;
	}

	if (line_ptr[0] == '+') 
		operator = 1;
	else if (line_ptr[0] == '-')
		operator = -1;
	else {
		fprintf(stderr, "%c is not valid operator\n", 
			line_ptr[0]);
		exit(EXIT_FAILURE);
	}
	printf("operator %d\n", operator);
*/
	

	
	
	} /* End of while(1) loop */

} /* End of main */

