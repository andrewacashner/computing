/* Oualline ch 5, exercise 5-4
*  Program asks user for mileage in kilometers per hour, gets and converts
*  number entered, converts to miles, and prints the result.
*  Formula: mph = kph * 0.6213712
*/

#include<stdio.h>

const float RATE = 0.6213712;	/* Conversion factor */
char line[10];	/* User input */
float kph;		/* Kilometers per hour, user input */
float mph;		/* Miles per hour, computed */

int main()
{
	printf("\nConvert your speed from kilometers per hour to miles per hour.\n\n");
	printf("Enter your speed in kilometers per hour: ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%f", &kph);

	mph = kph * RATE;

	printf("\nRESULT: %f kph = %f mph.\n\n", kph, mph);

	return(0);
}
