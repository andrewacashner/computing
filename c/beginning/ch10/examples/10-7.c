/* Oualline example 10-7 */


#include<stdio.h>
#include<stdlib.h>

#define DIE {fprintf(stderr, "Fatal Error:Abort\n");exit(8);}

int main() {
	
	char line[10]; 	/* Input buffer */
	int value; 		/* a random value for testing */

	printf("\nEnter positive integer or die: ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%d", &value);

#ifdef DEBUG
	printf("\nvalue = %d\n", value);
#endif /* DEBUG */

	if (value < 0)
		DIE;
	printf("We did not die\n");
	return(0);
}
