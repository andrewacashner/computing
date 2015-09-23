/* Oualline ch. 7, example 1: Trial calc program */

#include <stdio.h>

char line[100]; 	/* line of data from the input */
float result;			/* the result of the calculations */
char operator;		/* operator the user specified */
float value;			/* value specified after the operator */

int main()
{
	printf("\nFOUR-FUNCTION CALCULATOR");
	printf("\n\nUse operators +, -, *, /.");
	printf("\nType c to clear or q to quit.\n\n");

	result = 0; 	/* initialize the result */

	/* Loop forever (or till we hit the break statement) */

	while (1)
	{
		printf("Result: %.4f\n", result);

		printf("Calc:   ");
		fgets(line, sizeof(line), stdin);
		sscanf(line, "%c %f", &operator, &value);

		if ((operator == 'q') || (operator == 'Q'))
			break;

		if ((operator == 'c') || (operator == 'C')) {
			result = 0.0;
		} else if (operator == '+') {
			result += value;
		} else if (operator == '-') {
			result -= value;
		} else if (operator == '*') {
			result *= value;
		} else if (operator == '/') {
			if (value == 0) {
				printf("Error: Divide by zero\n");
				printf("Operation ignored\n");
			} else
				result /= value;
		} else {
			printf("Unknown operator %c\n", operator);
		}
	}
	return (0);
}
