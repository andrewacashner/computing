/* word_line -- print input from stdin one word per line (K&R 21) */

#include <stdio.h>

#define WHITE 1	/* Last character was whitespace */
#define BLACK 0 /* Last character was non-white */

int main(void)
{
	int c;				/* Input character (as int) */
	int last = WHITE;	/* Previous character: 1 = whitespace; 0 = char */

	/* Get character from standard input */
	while ((c = getchar()) != EOF) {
		
		/* Copy character if it is not whitespace */
		if (c != ' ' &&  c != '\t') {
			putchar(c);
			last = BLACK;

		} else if ((last == BLACK) && (c == ' ' || c == '\t')) {
			/* If it is the first whitespace, make new line */
			putchar('\n');
			last = WHITE;

		} else if (last == WHITE)
			; /* If multiple whitespace, do nothing */
	}
	return(0);
}
