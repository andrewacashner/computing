/* args.c -- Cashner 2014-11-15
 * Read command-line arguments and print them out
 * 
 * Expected usage: args [-<short optiongs>] [--<long options>] <filename(s)>
 *
 * Options:
 *	-v --version 	show version info and exit (default if no args)
 *	-h --help 	show help message and exit			
 *	-i --interactive prompt for filenames from stdin
 *	-r --reverse	list file names in reverse order
 *
 * -v, -h, -i cannot be combined; any args after these will be ignored
 * -i and -r can be combined 
 *
 * Arguments can be strung together like -ir or -ri
 * or can be separate like -i -r
 */
	/* - See if each argument is option (starts with hyphen), otherwise treat as
	 * filename. 
	 * - If is option, 
	 *	determine if short (-) or long (--) options
	 *	then read the option and store it
	 * - Read filenames (non-option arguments)
	 * - Print diagnostic to verify success
	 * - Exit with error message if args formatted incorrectly
	 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_OPTIONS 5

void exit_error(int);
void helpmsg(void);
void versionmsg(void);

int main(int argc, char *argv[])
{
	int i, j;
	enum { _name, _shortop, _longop } arg_type;
	enum { _none, _help, _version, _interactive, _reverse, _unknown } option = _none;
	char *arg_ptr;

	printf("Command has %d arguments.\n", argc);	
	
	if (argc == 1)
		versionmsg();
	
	for (i = 1; i < argc; ++i) {
		
		switch (argv[i][0]) {
			case '-':
				if (argv[i][1] == '-')
					arg_type = _longop;
				else arg_type = _shortop;
				break;
			default: 
				arg_type = _name;
		}
		
		printf("Argument %d is ", i); 
		switch (arg_type) {
			case _shortop:
				printf("a short option.\n");
				for (j = 1; argv[i][j] != '\0'; ++j) {
					switch (argv[i][j]) {
						case 'h':
							option = _help;
							break;
						case 'v':
							option = _version;
							break;
						case 'i':
							option = _interactive;
							break;
						case 'r':
							option = _reverse;
							break;
						default:
							exit_error(1);
					}
				}
				break;
			case _longop:
				printf("a long option.\n");
				arg_ptr = &argv[i][2];
				if (strcmp(arg_ptr, "help") == 0)
					option = _help;
				else if (strcmp(arg_ptr, "version") == 0)
					option = _version;
				else if (strcmp(arg_ptr, "interactive") == 0)
					option = _interactive;
				else if (strcmp(arg_ptr, "reverse") == 0)
					option = _reverse;
				else option = _unknown;
				break;
			case _name:
				printf("a filename.\n");
				break;
			default:
				printf("unknown.\n");
		}
	}	
	
	switch (option) {
		case _help:
			helpmsg();
			break;
		case _version:
			versionmsg();
			break;
		case _interactive:
			printf("Interactive mode selected.\n");
			break;
		case _reverse:
			printf("Reverse mode selected.\n");
			printf("Filenames given: ");
			for (j = argc - 1; j > 0; --j)
				printf("%s ", argv[j]);
			printf("\n");
			break;
		case _none:
			printf("Filenames given: ");
			for (j = 1; j < argc; ++j)
				printf("%s ", argv[j]);
			printf("\n");
			break;
		case _unknown:
			exit_error(1);
			break;
		default:
			exit_error(0);
	}

	return(0);
}

void exit_error(int error_type)
{
	switch (error_type) {
		case 1:
			fprintf(stderr, "Unknown option.\n");
			break;
		default:
			fprintf(stderr, "Error.\n");
	}
	fprintf(stderr, "Enter args -h for help.\n");
	exit(EXIT_FAILURE);
}

void helpmsg(void)
{
	printf("ARGS -- interpret command-line arguments\n");
	printf("Usage: args [-<short options>] [--<long options>]"
		"[filenames]\n");
	printf("Options:\n");
	printf("-h --help        Print this help message and exit\n");
	printf("-v --version     Print version information and exit\n");
	printf("-i --interactive Enter interactive mode:\n");
	printf("                   type filenames when prompted\n");
	printf("-r --reverse     Reverse mode: Print filenames in reverse\n");
	exit(0);
}

void versionmsg(void)
{
	printf("ARGS v0.1 by Andrew A. Cashner, 2014\n");
	exit(0);
}

