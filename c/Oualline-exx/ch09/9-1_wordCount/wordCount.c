/* WORD COUNT
*  Count number of words in string
*  Andrew Cashner, 2013-05-27
*  Oualline ch 9, exercise 1
*/

/* Ask for string input from user.
*  A word is defined as a continuous set of alphanumeric characters not
*  separated by a space. 
*  Program looks at each character in turn until it finds a space, then
*  increments a counter to get the number of words.
*/


#include<stdio.h>
#include<string.h>

/* Function to check if character is space */
int wordCount(char countLine[]) {

	int index;  	 /* counter for string character positions */
	int wordCounter = 1; /* counter for number of words */

	/* Loop through each character until the end-of string; if character is
	 * space, then add to word counter, if not, then keep looping. */

	for (index = 0; countLine[index] != '\0'; ++index) {
		if (countLine[index] == ' ') {
			++wordCounter;
			continue;
		}
		else continue;
	}
	return(wordCounter);
}

/* Function to see if first string begins same as second string; used to check
 * if quit or help is entered */

 int begins(char string1[], char string2[]) {

	char str1compare[100]; /* from string 1, to compare with string 2 */
	int  string2length;
	int  charNumber;

	string2length = strlen(string2); /*Get length of string to compare */

	/*Copy that number of characters from string1 into variable to compare */

	for (charNumber = 0; charNumber < string2length; ++charNumber) {
		str1compare[charNumber] = string1[charNumber];
	}
	str1compare[charNumber] = '\0'; /*Add end of string character */

/* Debug
	printf("string1 = %s, string2 = %s, str1compare = %s\n", 
	string1, string2, str1compare); 
*/
	if (strcmp(str1compare, string2) == 0) {
			return(0);
	} else return (1);
}

			
		

/*************************************************************/

int main() {

	char line[100]; 	/* Input line from user */

	printf("\nWORD COUNTER: Count words in string\n");

	while (1) {

		printf("\nEnter line: ");
		fgets(line, sizeof(line), stdin);

		if ((begins(line, "quit\n") == 0) || (begins(line, "q\n") == 0))
			break;
		else if ((begins(line, "help\n") == 0) || (begins (line, "h\n") == 0))
			printf("No help is available.\n");
		else if (begins(line, "ain\'t") == 0) {
			printf("\"ain\'t\" skipped cause ain\'t ain\'t a word.\n");
			printf("Line contains %d words.\n", (wordCount(line)-1));
		}
		else
			printf("Line contains %d words.\n", wordCount(line));
	}
	return(0);
}
