/* Euclid's algorithm for finding the greatest common divisor of two
 * integers */

#include <stdio.h>
#define FIRST	119
#define SECOND	544

int main(void)
{
	int m = FIRST;
	int n = SECOND;
	int r;

	do {
		r = m % n;
		m = n;
		n = r;
	} while (r != 0);
	
	printf("%d\n", m);

	return(0);
}
