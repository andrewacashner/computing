/* Oualline ch. 5, exercise 6, extra: Print all leap years
*  Andrew Cashner, 2012-02-24
* 
* Leap year is any year divisible by 4, not divisible by 100 (unless divisible
* by 400).
* 
* Program begins with any year chosen by user after 1583 (the first year after
* the Gregorian calendar reform began).
* Program ends with year chosen by the user.
*
* Program checks each year to see if it is leap year and if so, prints year.
* Checks to see if divisible by 4, then if also by 100, then if also by 400.
* If by 4 only, is leap year; if not, no.
* If by 4 and 100, is not leap year.
* If by 4 and 100 and 400, is leap year; if by 4 and 100 but not 400, is not.
*
* Program then increments year and repeats loop until year reaches the limit
* input by the user.
*/

#include<stdio.h>

char line[10]; 	/* Input buffer for start and end year */
int startYear;	/* User-input start year */
int endYear;	/* User-input end year */
int testYear;	/* Current year tested for leap year (begins at user choice) */
int leapTrue;	/* 0 if it IS a leap year (true), 1 if it is NOT. */

int main()
{
	printf("\n************************");
	printf("\n* LEAP-YEAR CALCULATOR *");
	printf("\n************************");
	printf("\n\nList all leap years between any two years (after 1582).");
	printf("\n\nEnter start year: ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%d", &startYear);

	printf("Enter end year:   ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%d", &endYear);

	printf("\n****\n");

	testYear = startYear;

	while (testYear < endYear)
	{
		if ( ((testYear / 4) > 0) && ((testYear % 4) == 0) )
		{
			leapTrue = 0;

			if ( ((testYear / 100) > 0) && ((testYear % 100) == 0) )
			{
				leapTrue = 1;

				if ( ((testYear / 400) > 0) && ((testYear % 400) == 0) )
				{
					leapTrue = 0;
				}
				else
				{
					leapTrue = 1;
				}
			}
			else
			{
				leapTrue = 0;
			}
		}
		else
		{
			leapTrue = 1;
		}

		if (leapTrue == 0)
		{
			printf("%d\n", testYear);
		}

		++testYear;
	}

	printf("****\n\n");

	return(0);
}
