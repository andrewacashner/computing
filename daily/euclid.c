/* Euclid's algorithm for computing largest common divisor
 * From Knuth TAOCP I
 * 2015-02-13
 */

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
	int m, n, r;	/* m, n = given inputs, r = remainder of m/n */
	
	/* Check command-line arguments */
	if (argc != 3) {
		fprintf(stderr, "Incorrect number of arguments.\n");
		fprintf(stderr, "Usage: euclid <number> <number>\n");
		return(EXIT_FAILURE);
	}
	
	/* Get values of m and n */
	sscanf(argv[1], "%d", &m);
	sscanf(argv[2], "%d", &n);
	
	/* DEBUG */
	printf("m = %d, n = %d\n", m, n);

	/* If n is larger than m, swap m and n */
	if (n > m) {
		n = m + n;
		m = n - m;
		n = n - m;
	}

	/* DEBUG */
	printf("m > n, so new m = %d, n = %d\n", m, n);

	while (1) {
		r = m % n;
		if (r == 0) {
			break;
		}
		m = n;
		n = r;
	}

	printf("The greatest common divisor of %s and %s is %d.\n", 
		argv[1], argv[2], n);

	return(0);
}
		


