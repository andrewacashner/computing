/* strep -- string repeat 
 * repeat given string given no. of times 
 *  Practice with reading and testing command line arguments, error handling
 * Cashner 2014-11-13 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX 100

void helpmsg(void);
void exit_errmsg(int);

int main(int argc, char *argv[])
{
	
	char line[MAX], phrase[MAX];
	int  repeats, i;
	enum {_interactive, _direct} mode;
	long l;
	char *endp;

	switch (argc) {
		case 2: /* -i for interactive mode, -h for help, else error */
			if (argv[1][0] == '-') {
				switch (argv[1][1]) {
					case 'i': 
						mode = _interactive;
						break;
					case 'h': 
						helpmsg();
						return(0);
					default: 
						exit_errmsg(2);
				}
			} else exit_errmsg(1);
			break;

		case 3: /* Direct mode with command-line arguments */
			/* Return error if last argument is not an integer */
			l = strtol(argv[2], &endp, 10);
			if (*endp) {
				exit_errmsg(3);
			} else mode = _direct;
			break;

		default:
			exit_errmsg(1);
	}

	switch (mode) {
		case _interactive:
			printf("Enter a phrase to repeat: ");
			fgets(line, sizeof(line), stdin);
			line[strlen(line) - 1] = '\0';
			strcpy(phrase, line);

			printf("Enter an integer number of times to repeat it: ");
			fgets(line, sizeof(line), stdin);
			sscanf(line, "%d", &repeats);
			break;

		case _direct:
			strcpy(phrase, argv[1]);
			sscanf(argv[2], "%d", &repeats);
			break;
	}

	for (i = repeats; i > 0; i--) 
		printf("%s ", phrase);
	printf("\n");
	
	return(0);
}

void helpmsg(void)
{
	printf("\nSTREP -- repeat a string a given number of times\n");
	printf("USAGE -- strep [OPTIONS] or strep \"<string>\" number\n");
	printf("Options:\n");
	printf("  -h display this help message\n");
	printf("  -i interactive mode with prompts for input\n\n");
	return;
}

void exit_errmsg(int code)
{
	switch (code) {
		case 1: 
			fprintf(stderr, "Incorrect arguments.\n");
			break;
		case 2: 
			fprintf(stderr, "Unrecognized option. "
				"Use -i, -h, or direct mode.\n");
			break;
		case 3: 
			fprintf(stderr, "Final argument is not an integer.\n");
			break;
		default: 
			; /* Do nothing */
	}
	fprintf(stderr, "Enter strep -h if you need help.\n");
	exit(EXIT_FAILURE);
}

