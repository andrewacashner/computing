/* Andrew's attempt based on Oualline ex. 5-6
*  The program asks for a first and last name. Then it computes the full
*  name in normal order (one variable) and in last-name-first order
*  (another variable). Then it prints both versions of the name, followed by
*  the initials. */

#include<stdio.h>
#include<string.h>

char first[100]; 			/* First name */
char last[100];				/* Last name */
char full_firstLast[200];	/* Full name (computed) in first-last order */
char full_lastFirst[200]; 	/* Full name (computed) in last-first order */

int main()
{
	printf("Please enter your first name: ");
	fgets(first, sizeof(first), stdin);
		/* Trim end-of-line character from name */
	first[strlen(first)-1] = '\0';

	printf("Please enter your last name: ");
	fgets(last, sizeof(last), stdin);
	last[strlen(last)-1] = '\0';

	strcpy(full_firstLast, first);
	strcat(full_firstLast, " ");
	strcat(full_firstLast, last);

	strcpy(full_lastFirst, last);
	strcat(full_lastFirst, ", ");
	strcat(full_lastFirst, first);

	printf("Your full name in first-last order is %s\n", full_firstLast);
	printf("Your full name in last-first order is %s\n", full_lastFirst);
	printf("Your initials are %c. %c.\n", first[0], last[0]);

	return(0);
}

