/* Andrew Cashner 2013-05-31
*  Oualline, ch. 9, exercise 3
*  Write a function count(number, array, length) that counts the number of
*  times number appears in array. The array has length elements. Function
*  should be recursive.
*/

#include<stdio.h>

int array[5];
int number; 	/*Number input by user to search string for */
int length; 	/* Length of array */

int count(int number, int array[], int length) {

	int index; 		/*for counting loop*/
	int total = 0; 		/*Number of occurences of "number" in array */

	for (index = 0; index < length; ++index) {
		if (array[index] == number)
			++total;
		else continue;
	}

	return(total);
}

int main() {

	char line[100]; /*User input buffer*/

	array[0] = 90;
	array[1] = 90;
	array[2] = 80;
	array[3] = 80;
	array[4] = 70;

	length = 5;

	printf("Enter number to search array for: ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%d", &number);
		
	printf("Number %d occurs %d times in array.\n", 
		number, count(number, array, length));

	return(0);

}
