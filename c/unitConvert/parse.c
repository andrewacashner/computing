/* PARSE: String parser, by Andrew Cashner 2013-03
*
*  Prototype for use in unitConvert program, to read units from input strings
*  like this: 43 cm to in OR 2.35 m to km OR 3 in2 to m2.
*  Counts number of characters in each word and then uses those numbers to copy
*  the correct characters to startUnit and endUnit variables.
*  
*  Reads input string. Trims end-of-line and trailing spaces. Uses loop to count
*  backward from end of string until it reaches a space; that count is assigned
*  to the last word. Then the count continues until another space is reached,
*  and this count is assigned to previous word.
************************************************************/

#include<stdio.h>
#include<string.h>

char  line[100]; 		/* User input buffer */
float startValue;		/* Starting numeric value pulled from input string */
int   repCounter; 		/* Counts repetitions of loop */
int   charCounter;		/* Counts characters */
int   endUnitLength;	/* Length of end unit input string */
int   startUnitLength; 	/* Length of start unit input string */
char  endUnit[100];		/* End unit for conversion */
char  startUnit[100];	/* Start unit for conversion */

/*************************************************************/

int main ()
{
	printf("\nInput desired conversion: ");
	fgets(line, sizeof(line), stdin);
	
	/* Get numeric value */
	sscanf(line, "%f", &startValue);

/************************************************************/

	/* Trim trailing characters from string */

		/*Trim end-of-line character \n */

	line[strlen(line)-1] = '\0';

		/*Trim trailing spaces */

	while (1) {
		if (line[strlen(line)-1] == ' ') {
			line[strlen(line)-1] = '\0';
		} else break;
	}

/* Debug */
	charCounter = strlen(line);
	endUnit[0] = line[charCounter-1];

	printf("\nString length %d", charCounter); 
	printf("\nLast character: %s\n", endUnit);
/**/

/************************************************************/

	/* Count backward from end of string to get number of characters per word */

	charCounter = strlen(line) - 1;

	for (repCounter = 0; repCounter < (strlen(line)); ++repCounter) {
	
		/* Look for end unit */		
		if (line[charCounter] == ' ') {

/* Debug */	printf("\nI found a space.\n");

			endUnitLength = repCounter; /* Get length of last word */
			charCounter = charCounter - 4;	/* Skip the space, then continue */

			/* Look for start unit (skip "_to_" in between) */
			for (repCounter = 0; repCounter < (strlen(line)); ++repCounter) {
				if (line[charCounter] == ' ') {

/* Debug */			printf("\nI found another space.\n");

					startUnitLength = repCounter; /* Length of first word */
					break; 
				} else {
					--charCounter;
					continue;
				} 
			}

			break;

		} else --charCounter;
	}

/* Debug */

	printf("\nendUnitLength %d, startUnitLength %d\n", endUnitLength,
	startUnitLength);

/**/

/************************************************************/

	/* Copy unit values */

		/* End Unit */
	
	for (repCounter = 0; repCounter < (endUnitLength); ++repCounter) {

		endUnit[repCounter] = line[strlen(line) - endUnitLength + repCounter];
/*Debug*/ printf("\n%d %c", repCounter, endUnit[repCounter]);
	}

	endUnit[endUnitLength] = '\0';

/*Debug*/ printf("\nendUnit = %s\n", endUnit);

		/* Start Unit */
	
			/* Move to beginning of start unit */
	charCounter = endUnitLength + startUnitLength + 4;

	for (repCounter = 0; repCounter < (startUnitLength); ++repCounter) {

		startUnit[repCounter] = line[strlen(line) - charCounter];
		--charCounter;

/*Debug*/ printf("\n%d %c", repCounter, startUnit[repCounter]);
	}	
		startUnit[startUnitLength] = '\0';

/*Debug*/ printf("\nstartUnit = %s\n", startUnit);

	
/************************************************************/
	return (0);
}
