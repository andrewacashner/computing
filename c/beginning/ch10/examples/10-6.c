/* Oualline example 10-6 */

#include <stdio.h>

#define SIZE 10
#define FUDGE SIZE -2

int main() {
	int size; /* Size to really use */

	size = FUDGE;
	printf("Size is %d\n", size);
	return(0);
}
