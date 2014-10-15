/* Oualline exercise 9-5: Write a function that returns the maximum value of an
 * array of numbers.
 * Andrew Cashner, 2013-06-30 */

 #include<stdio.h>

 /*************************************************************
 * Function: arrayMax
 *
 * Parameters: array of numbers, length of array
 * Returns: Maximum number in array
 *
 * Scans each position in array and compares it to previous.
 * Whichever of first pair is larger is stored in "largest" variable.
 * Then each next position is compared to "largest"; larger of those two is
 * stored in "largest."
 * Process ends at end-of-string character.
 ************************************************************/

unsigned long int arrayMax(unsigned long int array[], int length) {

	unsigned long int largest	= 0; /* Largest value */
	int index;		/* Position in array */

	for (index = 0; array[index] < length; ++index) {
		if (array[index] > largest) {
			largest = array[index];
		
			/* Debug */
			printf("\nFunction: array[%d] = %lu", index, array[index]);
			printf("\nFunction: largest = %lu", largest);
		}
	}

	return(largest);
}

/* Main: Get array of numbers */

int main() {

	char line[100]; 	 /* User input buffer */
	unsigned long int mainArray[5];	 /* Array to find largest number from */
	int count;			 /* for loop */

	printf("\nFind maximum number in array.\n");
	printf("\nEnter five integers separated by spaces: ");
	fgets(line, sizeof(line), stdin);

	sscanf(line, "%lu %lu %lu %lu %lu", 
		&mainArray[0], &mainArray[1], &mainArray[2], &mainArray[3],
		&mainArray[4]);
	
	/* Debug */

	printf("You entered: ");

	for (count = 0; count < 5; ++count) {
		printf("%lu ", mainArray[count]);
	}

	/**/

	printf("\nLargest integer is %lu.\n", arrayMax(mainArray, '5'));

	return(0);
}
			
 


