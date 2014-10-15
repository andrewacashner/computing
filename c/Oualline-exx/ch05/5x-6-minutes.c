/* Oualline ch 5, exercise 6
*  Hours to Minutes, by Andrew Cashner, 2012-02-15
*  Program requests a time in minutes from user,
*  gets string value and converts it to integer,
*  converts to hours and minutes, and prints result.
*  To convert, program takes minutes variable, divides by 60, gets remainder.
*  Minutes/60 = hours, remainder = minutes. 
*/

#include<stdio.h>

char line[10];	 	/* User input */
int inputMinutes;	/* Minutes input by user */
int hours;			/* Hours, calculated */
int outputMinutes;	/* Minutes, calculated */

int main()
{
	printf("\n******************");
	printf("\nTIME CONVERTER\n\n");
	printf("Input a time in minutes, to convert to hours and minutes: ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%d", &inputMinutes);

	hours = inputMinutes / 60;
	outputMinutes = inputMinutes % 60;

	printf("\nRESULT: %d minutes =  %d hours %d minutes.",
	inputMinutes, hours, outputMinutes);

	printf("\n\n******************\n\n");

	return(0);
}
