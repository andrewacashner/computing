/* Oualline exercise 10-2: Write a macro is_digit that returns TRUE if its
 * argument is a decimal digit. */

/* FAILURE: DON'T KNOW HOW TO DETERMINE IF ARGUMENT IS A DIGIT */

#include<stdio.h>

#define is_digit(x) ( ( (x) >= 0 ) || ( (x) <= 9) )

int main() {

	char line[5];	/* User input buffer */
	int number; 	/* Number to test if is digit */

	printf("\nEnter a single character: ");
	fgets(line, sizeof(line), stdin);
	
	sscanf(line, "%d", &number);
	if is_digit(number) {
		printf("\n%d is a digit.\n", number);
	} else printf("\nInput is not a decimal digit.\n");

	return (0);
}
