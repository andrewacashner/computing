/* Oualline ch. 6, exercise 2: Grading
*
*  Andrew Cashner, 2012-02-17
*
*  Program determines letter grades based on this numeric scale:
*
*  Program asks user to input numeric grade, converts input to integer.
*  Program uses loop to compare percent to specified percentRange and
*  determine which group of ten digits percentage falls
*  within (61-70, 71-80, etc): below 60 gets gradeTally of 0, and each higher
*  bracket gets one gradeTally higher as loop repeats (so 4 = A).
*
*  Program then determines whether grade falls within - zone or + zone by
*  comparing to percentRange (last digits 1-3 are -, last digits 8-0 are +);
*  then adds + or - to grade. Prints the result.
*/

#include<stdio.h>
#include<string.h>

char line[5]; 		/* Input buffer */
int percent; 		/* Percentage (numeric grade), input */
int gradeTally; 	/* Running total, used to calculate letters */
int gradeType; 		/* Negative for - grade, Positiv for + grade */
int percentRange; 	/* Used to compare to percent */
char grade[3]; 		/* Letter grade, calculated */

int main()
{
	printf("\nGRADE CALCULATOR\n\n");
	printf("Enter student's percentage grade (0–100): ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%d", &percent);

	gradeTally = 0;
	gradeType = 0;
	percentRange = 61;

	/* Compare percent to percentRange to see which letter range it falls
	 * within; if it is above range, then percentRange is increased,
	 * gradeTally is increased, and loop repeats. 
	 * If it is within range, loop ends, leaving gradeTally at current
	 * value. */

	while (gradeTally < 5) {

		if (percent >= percentRange) {
			percentRange += 10;
			++gradeTally;
			continue;
		}
		else break;
	}

	/* To determine whether to add + or - to grade */

	if (percent <= (percentRange - 8))
		gradeType = -1;
	if (percent >= (percentRange -3))
		gradeType = 1;
	if (percent < 61)
		gradeType = 0;

	/* Convert numeric grade codes to letter grades */

	if (gradeTally == 0) {
		strcpy(grade, "F");
	}
	if (gradeTally == 1) {
		strcpy(grade, "D");
	}
	if (gradeTally == 2) {
		strcpy(grade, "C");
	}
	if (gradeTally == 3) {
		strcpy(grade, "B");
	}
	if (gradeTally == 4) {
		strcpy(grade, "A");
	}

	/* Add + or - */

	if (gradeType < 0) {
		strcat(grade, "-");
	}
	if (gradeType > 0) {
		strcat(grade, "+");
	}

/*  For debugging:
*
*  printf("gradeType = %d, gradeTally = %d, percentRange = %d, percent =	%d\n\n", gradeType, gradeTally, percentRange, percent); 
*
*/

	printf("The student's grade is %s.\n\n", grade);

	return (0);
}
