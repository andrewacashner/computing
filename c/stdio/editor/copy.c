/* Copy program from K&R written in homemade WRITE */

#include <stdio.h>

/* Copy standard input to standard output */

int main(void)
{
	int c;

	while ((c = getchar()) != EOF)
		putchar(c);

	return (0);
}
