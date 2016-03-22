/* xmlpoem.c (Andrew Cashner, 2014-07-27)
 * Practice indexing through arrays of pointers, and of pointers to pointers;
 * print TEI XML output
 */

#include <stdio.h>

#define STANZAS	2		/* Number of stanzas given in advance */
#define LINES	8		/* Nr. of lines */

 int main(void) 
 {
 				 /* Plain text stored in sequential array */
 	char *lines[] = {	  "Jugó la primera mano",
				"y envidó con treintaitres,",
				"el que tres y uno es,",
				"en el juego soberano.",
				  "Jugaron en fin los dos",
				"hasta hora de cenar,",
				"que vino a ser el manjar,",
				"el mismo cuerpo de Dios."	};

	char **stanzas[STANZAS]; /* Array of addresses of start of each stanza */
	char **line_ptr;	 /* To index through lines[] */
	int s, l;		 /* Counters for stanzas and lines */
	
	/* Store addresses for start of each stanza */
	stanzas[0] = &lines[0];	
	stanzas[1] = &lines[4];

	/* See what is at the end of the array */
	line_ptr = lines;
	printf("%s\n", *(line_ptr + 7));

	/* Print all lines */
	for (line_ptr = lines; *(line_ptr + 1) != '\0'; line_ptr++)
		printf("%s\n", *line_ptr);

	/* Print XML version */
	printf("\n\n<poem lang=\"spa\">\n<original>\n");

	s = l = 0;
	for (line_ptr = lines; *(line_ptr + 2) != '\0'; line_ptr++) {
		
		if (line_ptr == stanzas[s]) {
			if (s > 0)
				printf("</stanza>\n");
			printf("<stanza>\n");
			s++;
		} 

		printf("<l n=%d>%s</l>\n", l + 1, *line_ptr);
		l++;
	}
	printf("</stanza>\n</original>\n</poem>\n\n");


	return(0);
}
