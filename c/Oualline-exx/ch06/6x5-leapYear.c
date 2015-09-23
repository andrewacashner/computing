/* Oualline ch. 6, exercise 5: Identify a leap year
*  Andrew Cashner, 2012-02-24
*
*  Leap year is any year divisible by 4 but not divisible by 100, unless it is
*  divisible by 400.
*
*  Program checks first if it is divisible by 4 (if so, sets true-false variable
*  to true (0), if not, to false); 
*  then checks to see if it is also divisible by 100 (if so, sets to false; if
*  not, to true);
*  then checks to see if it is also divisible by 400 (if so, sets to true; if
*  not, to false);
*  then based on true or false variable, changes string to "is" or "is not" and
*  prints whether it is a leap year.
*
*  Divisibility test is if when year is divided by divisor (4, 100, or 400), it
*  is divisible if integer quotient is greater than 0 AND if integer remainder
*  is greater than 0. Perhaps there is a better test?
*/

#include<stdio.h>
#include<string.h>

char line [10]; 		/* Input buffer */
int year;				/* Year input */
short int trueFalse;	/* 0 for true, 1 for false */

int main ()
{
	printf("\nLEAP YEAR CALCULATOR\n\n");

	printf("Enter a year to see if is a leap year: ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%d", &year);		/* Capture year input as integer */

/* If year is divisible by 4, is leap year until it passes remaining tests. */

	if ((year / 4) > 0)
	{
		if ((year % 4) == 0)	
		{
			trueFalse = 0;		
			
/* If year is divisible by 4 and by 100 is not leap year unless divisible by
 * 400. */
			
			if ((year / 100) > 0)
			{
				if ((year % 100) == 0)
				{

/* If year is divisible by 4 and by 100, but not 400, then not leap year. If
 * year is divisible by 4 and by 100, but is divisible by 400, then is leap
 * year. */
					
					if ((year / 400) > 0)
					{
						if ((year % 400) == 0)
						{
							trueFalse = 0;
						}
						else
						{
							trueFalse = 1;
						}
					}
					else
					{
						trueFalse = 1;
					}
				}

/* If year is divisible by 4 but not by 100, then is leap year. */
				
				else
				{
					trueFalse = 0;
				}
			}	
		}

/* If year is not divisible by 4, then is not leap year. */

		else
		{
			trueFalse = 1;
		}	
	}

/* If year is less than four, is not a leap year. */

	else
	{
		trueFalse = 1;
	}


/********************
*  Determine whether to print "is" or "is not" a leap year. 
********************/

	if (trueFalse == 0)
	{
		strcpy(line, "IS");
	}
	else
	{
		strcpy(line, "is NOT");
	}
	
	printf("\nRESULT: %d %s a leap year.\n\n", year, line);

	return(0);
}
