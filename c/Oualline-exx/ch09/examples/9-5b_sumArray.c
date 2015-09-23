/* Oualline ch 9 example after 9-5: Sum elements of array with recursive
 * function. */

#include <stdio.h>

 /*************************************************************
 * FUNCTION: Sum up elements of array between given start and end positions
 * 
 * PARAMETERS
 * 		first -- first element of array portion to sum up
 *  	last  -- last element of array portion to sum up
 * 		array -- array of numbers to sum up
 *
 * RETURNS
 * 		sum of elements
 *
 * Recursive function: adds up pairs of numbers until "first" position = "last"
 * position; that is, until the end of the given array portion
 ************************************************************/

 int sum(int first, int last, int array[]) {

/* Debug 
	printf("\nfirst %d, last %d", first, last);
	printf("\narray[first] = %d", array[first]);
	printf("\narray[last] = %d", array[last]); */


 	if (first == last) {
		printf("\nfirst = last, array[first] = %d", array[first]);
		return (array[first]);
	}
	else {
		/* Debug	
		printf("\nfirst != last, go recursive function:");
		printf("\nsum(first+1, last, array) = %d", sum(first+1, last, array));
		*/

		return (array[first] + sum(first+1, last, array));
	}
}

int main() {

	char line[25];		/* User input buffer */
	int main_array[5];  /* Array entered by user */
	int first_position; /* Start position (e.g., main_array[0]) for sum */
	int last_position; 	/* End position for sum*/

	/* Get values for array */
	printf("\nEnter 5 integers to store in array, separated by spaces: ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%d %d %d %d %d", 
		&main_array[0], &main_array[1], 
		&main_array[2], &main_array[3], &main_array[4]);

	/* Get start and end positions for summation */
	printf("\nSum array values between given start and end position.");
	printf("\nEnter start and end position (0–4), separated by space: ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%d %d", &first_position, &last_position);

	/* Get and print total using function "sum"*/
	printf("\nSum of values between position %d and position %d = %d\n",
		first_position, last_position, 
		sum(first_position, last_position, main_array));

	return(0);
}



