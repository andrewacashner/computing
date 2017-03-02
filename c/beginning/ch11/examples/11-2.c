/* Oualline example 11-2 */

#include <stdio.h>

const int HIGH_SPEED = (1<<6);	/* modem is running fast */

/* we are using a hardwired connection */
const int DIRECT_CONNECT = (1<<7);

char flags = 0;	/* start with nothing */

int main() {

	flags |= HIGH_SPEED;	/* we are running fast */
	flags |= DIRECT_CONNECT; /* because we are wired together */

	if ((flags & HIGH_SPEED) != 0)
		printf("High speed set\n");
	
	if ((flags & DIRECT_CONNECT) != 0)
		printf("Direct connect set\n");
	
	return (0);
}
