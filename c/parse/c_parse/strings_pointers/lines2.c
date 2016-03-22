/* lines2.c	Andrew Cashner	2014-07-11
 * Practice reading and storing strings with pointer arrays
 * Now array of pointers
 */

#include <stdio.h>
#include <string.h>

int main(void) 
{
	char *stanza[] = { 	"Jugó la primera mano",
					 	"y envidó con treintaitres",
					 	"el que tres y uno es",
					 	"en el juego soberano." };
	int i;

	for (i = 0; stanza[i + 1] != '\0'; i++) {
		printf("%s", stanza[i]);
		printf("\n");
	}

	return (0);
}
