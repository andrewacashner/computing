/* Oualline ch. 6, exercise 2: Grading
*  Andrew Cashner, 2012-02-16
*  Program determines letter grades based on this numeric scale:
*  
*  PERCENT	GRADE
*  0-60		F
*  61-70	D
*  71-80	C
*  81-90	B
*  91-100	A
*
*  Program asks user to input numeric grade, converts input to integer.
*  Program uses if-else clauses to determine the grade and prints result.
*/

#include<stdio.h>
#include<string.h>

char line[5]; 		/* Input buffer */
int percent; 	/* Percentage (numeric grade), input */
char grade[3]; 		/* Letter grade, calculated */

int main()
{
	printf("\nGRADE CALCULATOR\n\n");
	printf("Enter student's percentage grade (0–100): ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%d", &percent);

	if (percent < 61)
		strcpy(grade, "F");
	if ((percent >= 61) && (percent <= 63))
		strcpy(grade, "D-");
	if ((percent >= 64) && (percent <= 67))
		strcpy(grade, "D");
	if ((percent >= 68) && (percent <= 70))
		strcpy(grade, "D+");

printf("The student's grade is %s.\n\n", grade);

	return (0);
}
