/* Oualline, chapter 6, example 1: Uses loop to compute Fibonacci sequence */
/* Modified by Andrew Cashner */

#include <stdio.h>

char line[1000000];		/* Input buffer */
long int limit; 		/* Upper limit, entered by user */
int old_number = 1; 	/* previous Fibonacci number */
int current_number = 1;	/* current Fibonacci number */
int next_number;		/* next number in the series */

int main()
{
	printf("\nFIBONACCI SEQUENCE CALCULATOR\n");
	printf("Enter upper number limit (max 1000000): ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%ld", &limit); /* Get upper limit */

	printf("1 ");	/* Print first number */

	while (current_number <= limit)
		{
		printf("%d ", current_number);
		next_number = current_number + old_number;

		old_number = current_number;
		current_number = next_number;
		}

	printf("\n");

	return (0);
}
