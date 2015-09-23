#include <stdio.h>

#define BASE 17
#define MAX  1000

int main(void)
{
	int n = BASE;
	int i;

	for (i = 0; n < 1000; i++, n = n + BASE)
		printf("%d ", n);
	
	return (0);
}
