/* Oualline ch 6 (branching conditional statements)
*  Time Converter, by Andrew Cashner, 2012-02-16
* 
*  Program asks user whether to convert minutes to hours and minutes or vice
*  versa. If conversion is minutes to hours, the program takes
*  To convert, program takes minutes-input variable, divides by 60, gets
*  remainder. Minutes/60 = hours, remainder = minutes. 
*
*  If conversion is hours and minutes to just minutes, 
*  program takes hours variable and multiplies by 60 to get
*  minutes (storing the new value in a new variable for total
*  minutes), and adds the minutes to the new variable.
*/

#include<stdio.h>

char line[10];	 			/* User input */
short int conversionChoice;	
	/* User input: 1 for min. to hours+min.; 2 for reverse */
int onlyMinutesInput;	/* Minutes-only input, to be converted to hours, min. */
int hoursOutput; 		/* Hours, calculated */
int remainderMinutesOutput; /* Minutes (not incl. hours), calculated */
int hoursInput;				/* Hours input by user */
int remainderMinutesInput;	/* Minutes (not incl. hours) input by user */
int onlyMinutesOutput;		/* Total in just minutes, calculated */

int main()
{
	printf("\n******************");
	printf("\nTIME CONVERTER\n\n");
	printf("Convert minutes only to hours and minutes (select 1)\n");
	printf("OR convert hours and minutes to minutes only (select 2).\n\n");
	printf("Select your conversion type (1 or 2): ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%hd", &conversionChoice);

	if (conversionChoice == 1)
		{		
		printf("Input a time in minutes, to convert to hours and minutes: ");
		fgets(line, sizeof(line), stdin);
		sscanf(line, "%d", &onlyMinutesInput);

		hoursOutput = onlyMinutesInput / 60;
		remainderMinutesOutput = onlyMinutesInput % 60;

		printf("\nRESULT: %d minutes = %d hours and %d minutes.\n",
		onlyMinutesInput, hoursOutput, remainderMinutesOutput);

		printf("\n******************\n\n");
		}
	else
		{
		printf("Input hours and minutes and calculate total minutes.\n");
		printf("Enter time in the format # h # m : ");
		fgets(line, sizeof(line), stdin);
		sscanf(line, "%d h %d m", &hoursInput, &remainderMinutesInput);
		
		onlyMinutesOutput = hoursInput * 60 + remainderMinutesInput;

		printf("\nRESULT: %d hours and  %d minutes = %d minutes.\n", 
		hoursInput, remainderMinutesInput, onlyMinutesOutput);
		printf("\n******************\n\n");
		}

	return(0);
}
