/* arg_test.c -- Cashner 2014-11-15
 * Alternate attempt, read cli args
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_INPUT 100
#define MAX_NAMES 50


int main(int argc, char *argv[])
{
	enum { _filename, _option } arg_type;
	enum { _none, _version, _help, _interactive, _reverse_interactive, 
		_file, _reverse_file, _error } mode;	

	char *mode_name[] = {"none", "version", "help", "interactive", 
				"reverse_interactive", "file",
				"reverse_file", "error"};
	int i, first_file_arg;
	
	char line[MAX_INPUT];
	char filenames[MAX_NAMES], next_reverse_name[MAX_NAMES];
	int  max_files;

	switch (argc) {
		case 1: 
			mode = _version;
			break;
		case 2:
			if (argv[1][0] == '-')
				arg_type = _option;
			else {
				arg_type = _filename;
				mode = _file;
				first_file_arg = 1;
				break;
			}
			if (arg_type == _option) {
				if ((strcmp(argv[1], "-h") == 0) ||
				    (strcmp(argv[1], "--help") == 0))
				    	mode = _help;
				else if ((strcmp(argv[1], "-v") == 0) ||
					(strcmp(argv[1], "--version") == 0))
					mode = _version;
				else if ((strcmp(argv[1], "-i") == 0) ||
					(strcmp(argv[1], "--interactive") == 0))
					mode = _interactive;
				else if ((strcmp(argv[1], "-ir") == 0) ||
					(strcmp(argv[1], "-ri") == 0))
					mode = _reverse_interactive;
				else mode = _error;
			}
			break;
		default: /* 3 or more */
			if (argv[1][0] == '-')
				arg_type = _option;
			else {
				arg_type = _filename;
				mode = _file;
				first_file_arg = 1;
				break;
			}
			if (arg_type == _option) {
				if ((strcmp(argv[1], "-i") == 0) ||
			 	    (strcmp(argv[1], "--interactive") == 0)) {
					if ((strcmp(argv[2], "-r") == 0) ||
					    (strcmp(argv[2], "--reverse") == 0))
					   	mode = _reverse_interactive;
					else mode = _error;
				} else if ((strcmp(argv[1], "-r") == 0) ||
				     (strcmp(argv[1], "--reverse") == 0)) {
				     	if ((strcmp(argv[2], "-i") == 0) ||
					(strcmp(argv[2], "--interactive") == 0))
						mode = _reverse_interactive;
					else {
						mode = _reverse_file;
						first_file_arg = 2;
					}
				} else mode = _error;
			}
	}
	
	printf("MODE: %s\n", mode_name[(int)mode]);

	switch (mode) {
		case _version:
			printf("ARG_TEST v0.1 by Andrew Cashner, 2014\n");
			printf("Type arg_test -h for help.\n");
			break;
		case _help:
			printf("ARG_TEST -- read command-line arguments.\n");
			printf("In normal mode, ARG_TEST will print the names \n");
			printf("of files given as arguments.\n");
			printf("In interactive mode, it will prompt for ");
			printf("the names.\n");
			printf("In reverse mode, it will print the names in ");
			printf("reverse order. Both normal and interactive ");
			printf("modes can be put in reverse.\n\n");
			printf("Usage: arg_test [OPTIONS] <filenames>\n");
			printf("  Options:\n");
			printf("  -v --version Print version info, exit\n");
			printf("  -h --help    Print this help message, exit\n");
			printf("  -i --interactive Interactive mode\n");
			printf("  -r --reverse     Reverse mode\n");
			printf("Interactive and reverse options can be combined.\n");
			break;
		case _error:
			printf("Incorrect syntax. Type arg_test -h for help.\n");
			exit(EXIT_FAILURE);
			break;
		case _file:
			for (i = first_file_arg; i < argc; ++i)
				printf("%s\n", argv[i]);
			break;
		case _reverse_file:
			for (i = argc; i > first_file_arg; --i)
				printf("%s\n", argv[i - 1]);
			break;
		case _interactive:
			printf("How many file names are there? (number) ");
			fgets(line, sizeof(line), stdin);
			sscanf(line, "%d", &max_files);
			for (i = 0; i < max_files; ++i) {
				printf("Enter file name %d: ", i + 1);
				fgets(line, sizeof(line), stdin);
				strcat(filenames, line);
			}
			printf("%s",  filenames);
			break;
		case _reverse_interactive:
			printf("How many file names are there? (number) ");
			fgets(line, sizeof(line), stdin);
			sscanf(line, "%d", &max_files);
			for (i = 0; i < max_files; ++i) {
				printf("Enter file name %d: ", i + 1);
				fgets(line, sizeof(line), stdin);
				strcpy(next_reverse_name, line);
				strcat(next_reverse_name, filenames);
				strcpy(filenames, next_reverse_name);
			}
			printf("%s", filenames);
			break;
		default:
			printf("I don't know what to do.\n");
	}
	

	return(0);
}

