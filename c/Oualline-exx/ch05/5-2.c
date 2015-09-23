/* Oualline ex. 5-2: Strings */

#include <string.h>
#include <stdio.h>

char name[30];	/* First name of someone */

int main()
{
	strcpy(name, "Sam"); 	/* Initialize the name */
	printf("The name is %s\n", name);

	return (0);
}
