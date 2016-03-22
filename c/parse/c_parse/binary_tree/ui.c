/* ui.c -- basic user input refresher, 2014-08-29 */

#include <stdio.h>
#include <string.h>

int main (void)
{
	char line[100];
	char input[100];

	printf("Enter a string: ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%s", input);

	printf("You entered: %s.\n", input);

	return(0);
}

