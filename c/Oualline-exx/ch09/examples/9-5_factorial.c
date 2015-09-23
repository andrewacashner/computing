/* Oualline 9-5 Recursive functions, factorial function */

/*****************************************************************************
* Factorial: Calculates factorial
*
* Parameters
*   number to calculate factorial of
* 
* Returns
*   factorial of the number
*   (that is, the product of all the integers smaller than it, given that the
*   factorial of 0 is 1.)
* Cannot compute negative numbers
*****************************************************************************/

#include <stdio.h>

unsigned long int fact(int number) {

	if (number >= 0) {
		if (number == 0)
			return (1);
		else 
			return (number * fact(number-1));
	} else return (0); /* 0 used as sign that factorial could not compute */
}

int main() {

	char line[100]; 	/* User input buffer */
	int  entryNumber;   /* Number to be calculated */


	printf("\nFACTORIAL CALCULATOR\n");

	while (1) {

		printf("\nEnter number (q to quit): ");
		fgets(line, sizeof(line), stdin);

		if (line[0] == 'q') /* Check for quit */
			break;
		else {
			sscanf(line, "%d", &entryNumber);
		
			/* Make sure there is no computing error (negative input, too
			 * large output */

			if (fact(entryNumber) != 0) 
				printf("%d! = %lu\n", entryNumber, fact(entryNumber));
			else
				printf("Error: Cannot compute.\n");
		}
	}

	return(0);
}
