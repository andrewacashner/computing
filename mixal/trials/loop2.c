/* C version of loop2.mix */

#include <stdio.h>
#define BEGIN 0
#define INCREMENT 2
#define MAX 11

int main(void)
{
	int m, n;
	while (m < MAX) {
		n = m;
		m = m + INCREMENT;
	}
	printf("%d\n", n);
	return (0);
}

