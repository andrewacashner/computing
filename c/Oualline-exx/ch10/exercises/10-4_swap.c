/* Oualline exercise 10-4: Write a preprocessor macro that swaps two
 * integers. (Bonus: Write one that does not use a temporary variable
 * declared outside the macro.) */

#include<stdio.h>

/* Swap two integers */
#define swap(x, y) { int temp = x; x = y; y = temp; } 

int main() {
	
	char line[100]; 	/* Input buffer */
	int numOne;	/* First integer */
	int numTwo; /* Second integer */

 	printf("\nEnter two integers separated by a space: ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%d %d", &numOne, &numTwo);
	printf("\nx = %d, y = %d", numOne, numTwo);

	swap(numOne, numTwo)

	printf("\nSWAP variables:");
	printf("\nx = %d, y = %d\n", numOne, numTwo);

	return(0);
}
