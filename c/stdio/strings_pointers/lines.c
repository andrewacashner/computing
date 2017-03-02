/* lines.c	Andrew Cashner	2014-07-11
 * Practice reading and storing strings with pointer arrays
 */

#include <stdio.h>
#include <string.h>

int main(void) 
{
	char line0[50] = "Jugó la primera mano";
	char line1[50] = "y envidó con treintaitres";
	char line2[50] = "el que tres y uno es";
	char line3[50] = "en el juego soberano.";
	char *stanza[5];
	int  i;
	char *c_ptr;

	stanza[0] = line0;
	stanza[1] = line1;
	stanza[2] = line2;
	stanza[3] = line3;
	
	for (i = 0; i < 4; ++i) {
		for (c_ptr = stanza[i]; *c_ptr != '\0'; ++c_ptr)
			printf("%c", *c_ptr);
	}
	printf("\n");

	return (0);
}
