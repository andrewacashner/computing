/* LETTERS (Andrew Cashner, 2014-06-21)
 *
 * This program will display large ASCII-art versions
 * of each character typed by the user.
 *
 * The input will be read with a simple getchar function.
 * That is, computer waits for the user to type a character, and then when
 * the user types a character, the computer stores the character.
 * Next the program draws the ASCII-art character corresponding to the one
 * typed. 
 *
 * Each letter has hard-coded instructions for drawing it as vectors, so setting
 * start and end points for drawing each line necessary.
 * For curves we will have to define how to draw a generic curve and then
 * specify the size (for now we stick to straight-line letters).
 * 
 * The bitmap function translates the vectors into a binary bitmap.
 * The drawing function draws the bitmap with (for now) 'X' for 1 and ' '
 * (space) for 0.
 *
 * We will have to include a delay so that each letter stays on the screen for a
 * minimum amount of time.
 *
 */

#include <stdio.h>

int main(void)
{

	char line[100];	/* User input buffer */
	char c;			/* Character to display */

	/* Greet the user and prompt for input */
	printf("LETTERS: Type a letter and see it drawn larger.\n");
	
	while (1) {
		printf("\n");
		fgets(line, sizeof(line), stdin);
		c = (int)line[0];
		if (c == EOF)
			break;
		else
			putchar(c);
	}

	return (0);
}
