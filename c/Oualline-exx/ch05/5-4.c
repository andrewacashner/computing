/* Oualline ex. 5-4: Read a line from the keyboard and report its length */

#include <string.h>
#include <stdio.h>

char line[100]; 	/* Line we are looking at */
int lineLength;		/* My addition: length of line */

int main()
{
	printf("Enter a line: ");
	fgets(line, sizeof(line), stdin);

	lineLength = strlen(line);

	printf("The length of the line is: %d\n", lineLength);

	return (0);
}
