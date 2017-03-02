/* lines3.c	Andrew Cashner	2014-07-11
 * Practice reading and storing strings with pointer arrays
 * Now array of pointers with pointer arithmetic (K/R p. 115)
 */

#include <stdio.h>

int main(void) 
{
	char *stanza[] = { 	"Jugó la primera mano",
					 	"y envidó con treintaitres",
					 	"el que tres y uno es",
					 	"en el juego soberano." };
	char **stanza_ptr = stanza;

	while (*(stanza_ptr + 1) != '\0') {
		printf("%s", *stanza_ptr);
		printf("\n");
		stanza_ptr++;
	}

	return (0);
}
