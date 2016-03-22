/* Usage: range <rangetag> <start> <end>
 * Examples: 
 * 	range measures 13 14 --> mm.~13--14
 *	range measures 13    --> m.~13
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MEASURETAG 'm'
#define LINETAG	'l'
#define TAGPUNCT '.'

int main(int argc, char *argv[])
{
	char rangetag;
	char tagpunct = TAGPUNCT;

	if ((argc > 4) || (argc < 3)) {
		fprintf(stderr, "Incorrect number of arguments.\n");
		exit(EXIT_FAILURE);
	}

	if (strcmp(argv[1], "measures") == 0) 
		rangetag = MEASURETAG;
	else if (strcmp(argv[1], "lines") == 0)
		rangetag = LINETAG;
	else {
		fprintf(stderr, "Unknown range label.\n");
		exit(EXIT_FAILURE);
	}
	
	if (argc == 4)  /* If there are start and end range arguments */
		printf("%c%c%c~%s--%s\n",
			rangetag, rangetag, tagpunct, argv[2], argv[3]);
	else /* If only one range argument */
		printf("%c%c~%s\n", rangetag, tagpunct, argv[2]);
	
	return(0);
}
