/* OPTIONS (Andrew Cashner, 2014-03-01) Play with reading options from the
 * command line */

#include <stdio.h>
#include <stdlib.h>

int main (int argc, char *argv[])
{
	int i, j;			/* Loop counters */
	char optshort;		/* Short option: One letter */
	char optlong[25];	/* Long option: String  */

	if (argc > 6) {
		fprintf(stderr, "Too many arguments. Usage: <options> [options]\n"); 
		fprintf(stderr, "Possible options: -a -b -c -d -e\n");
		exit(8);
	}

	printf("%d arguments detected.\n", argc - 1);

	/* Read each argument (ignoring argv[0], which is program name) */
	for (i = 1; i < argc; ++i) {

/* DEBUG */
		if ( (argv[i][0] == '-') && (argv[i][1] != '\0') ) {
			printf("Option: %s\n", argv[i]);
		} else printf("Incorrect option: %s\n", argv[i]);
/* end DEBUG */

		/* Eliminate errors, determine if short or long option, get option */
		if ((argv[i][0] == '-') && (argv[i][1] != '\0') && (argv[i][2] == '\0')) {
				optshort = argv[i][1];
				printf("Short option: %c\n", optshort);
		}
		else if ((argv[i][0] == '-') && (argv[i][1] == '-') && (argv[i][2] != '\0')) {
			for (j = 2; argv[i][j - 1] != '\0'; ++j) {
					optlong[j - 2] = argv[i][j];
			}
			printf("Long option: %s\n", optlong);
		} else printf("Incorrect option: %s\n", argv[i]);

	}	/* End of for loop */

/* Next: store options and print list */

	return(0);
}

