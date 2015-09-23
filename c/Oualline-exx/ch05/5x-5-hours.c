/* Oualline ch 5, exercise 5 
*  Hours to Minutes, by Andrew Cashner, 2012-02-13
*  Program requests a time in hours and minutes from user (input as # h # m),
*  gets string values and converts them to integers,
*  converts hours and minutes to just minutes, and prints result.
*  To convert, program takes hours variable and multiplies by 60 to get
*  minutes (storing the new value in a new variable for total
*  minutes), and adds the minutes to the new variable.
*/

#include<stdio.h>

char line[10];	 	/* User input */
int hours;			/* Hours input by user */
int onlyMinutes;	/* Minutes input by user */
int totalMinutes;	/* Total in just minutes, calculated */

int main()
{
	printf("\n******************");
	printf("\nMINUTES CALCULATOR\n\n");
	printf("Input a time in hours and minutes and calculate total minutes.\n");
	printf("Enter hours and minutes. Use the format # h # m :\n");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%d h %d m", &hours, &onlyMinutes);

	totalMinutes = hours * 60 + onlyMinutes;

	printf("\nRESULT: %d h %d m = %d m.", 
	hours, onlyMinutes, totalMinutes);
	printf("\n\n******************\n\n");

	return(0);
}
