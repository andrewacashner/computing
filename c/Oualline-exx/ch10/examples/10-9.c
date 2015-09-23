/* Oualline example 10-9 */

#include <stdio.h>

#define SQR(x) ((x) * (x))

int main() {

	int counter; 	/* counter for loop */

	for (counter=1; counter < 5; ++counter)
		printf("x %d square %d\n", counter, SQR(counter));
	
	return (0);
}
