/* Oualline exercise 9-3: Count the number of times number appears in array.
 * Should be recursive.*/

#include <stdio.h>

/*************************************************************
* FUNCTION count : counts number of times a number appears in an array
* 
* PARAMETERS
*    number : number to search for
*    array  : array to search in
*    length : length of array
* 
* Returns
*    number of occurences
*************************************************************/

int count(int number, int array[], int length) {

	int total = 0; 	/* Total of occurrences of number in array */

	if (length == 0)
		return (0);
	else {
		if (array[length-1] == number) {
			++total;
		}
		return (total + count(number, array, length-1));
	}
}
		
/************************************************************/

int main() {

	int counter; 		/* Count total numbers in the array */
	char line[100]; 	/* User input buffer */
	int main_array[100]; /* Array of numbers to be input by user */
	int search_number;  /* Number to search for */

	printf("\nCount occurences of a number in an array.\n");
	
	printf("\nEnter five array values:\n");

	for (counter = 0; counter < 5; ++counter) {
		printf("Array[%d] = ", counter);
		fgets(line, sizeof(line), stdin);
		sscanf(line, "%d", &main_array[counter]);
	}
	
	printf("\nEnter number to search array for: ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%d", &search_number);

	printf("\nNumber %d occurs %d times in array.\n",
		search_number, count(search_number, main_array, counter));

	return(0);
}
