/* Copy */

#include <stdio.h>
#include <stdlib.h>

int main (void) {

	int c;
	
	while ( ( (c = getchar()) != EOF) && (c != 'Q') )
		putchar(c);
	
	return (0);
}
