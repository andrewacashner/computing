/* C version of loop3.mix */

#include <stdio.h>
#define BEGIN 0
#define INCREMENT 2
#define MAX 11

int main(void)
{
	int n;
	for (n = 0; n < MAX; n = n + INCREMENT)
		; /* that's all */
	printf("%d\n", n - INCREMENT);
	return (0);
}

