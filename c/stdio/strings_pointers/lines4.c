/* lines4.c	Andrew Cashner	2014-07-11
 * Practice reading and storing strings with pointer arrays
 * Now array of pointers with pointer arithmetic (K/R p. 115)
 * Now adding an array of pointers to pointers!
 */

#include <stdio.h>
#define MAX_STANZAS 2
#define LINES_PER_STANZA 4

int main(void) 
{
	char *lines[] = { 	"Jugó la primera mano",
			 	"y envidó con treintaitres",
			  	"el que tres y uno es",
			  	"en el juego soberano.",

				"Jugaron en fin los dos",
				"hasta hora de cenar",
				"que el mismo cuerpo de Dios",
				"vino a ser el manjar."
			};
	char **line_ptr;
	char **stanzas[MAX_STANZAS];
	char **stanza_ptr;
	int i, j;
	
	stanzas[0] = &lines[0];
	stanzas[1] = stanzas[0] + LINES_PER_STANZA;
	
	printf("\nPrint whole text by indexing through LINES:\n\n");
	for (line_ptr = lines; *line_ptr != NULL; line_ptr++) 
		printf("%s\n", *line_ptr);
	
	printf("\nPrint first lines by indexing through STANZAS:\n");
	for (i = 0; i < MAX_STANZAS; i++) 
		printf("%s\n", *stanzas[i]);

	printf("\nIndex through stanza_ptr by 1s instead:\n");
	for (stanza_ptr = *stanzas; *stanza_ptr != NULL; stanza_ptr++)
		printf("%s\n", *stanza_ptr);
	
	printf("\nIndex through stanza_ptr by stanza lengths instead:\n");
	for (stanza_ptr = *stanzas; *stanza_ptr != NULL;
 	      stanza_ptr = stanza_ptr + LINES_PER_STANZA)
		printf("%s\n", *stanza_ptr);

	printf("\nPrint XML for this poem:\n");
	printf("<poem lang=spa>\n");

	for (line_ptr = lines, i = j = 0; *(line_ptr + 1) != NULL;
	     line_ptr++, j++) {

		if (stanzas[i + 1] != '\0') {
			if (line_ptr == stanzas[i])
				printf("<stanza n=%d>\n", ++i);
			else if (line_ptr + 1 == stanzas[i + 1]) {
				printf("</stanza>\n");
				i++;
			}
		}

		printf("<l n = %d>%s</l>\n", j + 1, *line_ptr);
	}

	printf("</poem>\n");

	printf("\n");
	return (0);
}
