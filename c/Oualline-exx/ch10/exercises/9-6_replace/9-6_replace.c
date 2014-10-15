/* Oualline exercise 9-6, Andrew Cashner, 2013-07-04
*  Write a function that scans a character array for the character - and
*  replaces it with _. */

#include <stdio.h>
#include <string.h>

/************************************************************
* Function: Replace
*
* Scans character array for one character and replaces with another.
*
* Parameters: character array, "find" character, "replace" character
*
* Return: modified character array
*************************************************************/

void replace(char string[], char find, char replace) {

	int index; 	/* Counter for char-array positions */

	for (index = 0; string[index] != '\0'; ++index) {
		if (string[index] == find)
			string[index] = replace;
	}
	return;

}

/************************************************************/

int main() {

	char line[100]; /* Input buffer */
	char text[100]; /* Text input */
	char oldChar;   /* Character to find */
	char newChar;   /* Character to replace */

	printf("\nFIND AND REPLACE\n");
	printf("\nEnter text: ");
	fgets(line, sizeof(line), stdin);
	strcpy(text, line);

	printf("\nCharacter to find: ");
	fgets(line, sizeof(line), stdin);
	oldChar = line[0];

	printf("\nCharacter to replace with: ");
	fgets(line, sizeof(line), stdin);
	newChar = line[0];

	replace(text, oldChar, newChar);

	printf("\nNew text: %s\n", text);

	return(0);
}

	

