/* Oualline ex 5-3: Store full name in string */

#include <string.h>
#include <stdio.h>

char first[100];		/* first name */
char last[100];			/* last name */
char full_name[200];	/* full version of first and last name */

int main()
{
	strcpy(first, "Steve");		/* Initialize first name */
	strcpy(last, "Oualline");	/* Initialize last name */

	strcpy(full_name, first);	/* full = "Steve" */
	
	strcat(full_name, " ");		/* full = "Steve " */
	strcat(full_name, last);	/* full = "Steve Oualline" */

	printf("The full name is %s\n", full_name);

	return (0);
}
