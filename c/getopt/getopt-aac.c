/* getopt-aac.c -- my practice with short options with getopt
 * 2016/12/01
 *
 * Prints a message to specified output file;
 * Message can be specified with -e (expression) option
 * 
 * Usage:
 *
 * getopt [OPTIONS] FILE
 *
 * Options:
 *  -h          Print help message
 *  -v          Print version message
 *  -e <STRING> Specify string to print to output file
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <getopt.h>

#define MAX_STRING 16*1000

char default_output_message[] =
     "I think the most important thing is that I have faith,\n"
     "and that I have tried to express that faith through my music.\n"
     "That is all that matters.  --Olivier Messiaen\n";
char default_outfile_name[] = "getopt-aac-output.txt";

const enum {
     HELP,
     VERSION,
     OPTION_WITHOUT_ARGUMENT,
     OPTION_UNKNOWN,
     TOO_MANY_ARGUMENTS,
     NO_OUTFILE_SPECIFIED,
     OUTFILE_OPEN_FAILURE
} message_codes;

const char *message[] = {
     "Help message.",
     "Version message.",
     "Option -%s requires an argument.",
     "Unknown option '-%s'"
     "No output file specified.",
     "Could not open file %s for writing."
};

/* FUNCTION PROTOTYPES */
void quit_msg(int message_code);
void quit_error_msg(int message_code, char detail_str[]);


int main(int argc, char *argv[])
{
     int c;
     FILE *outfile;
     char *outfile_name = default_outfile_name;
     char *print_string = NULL;
     char *error_msg_detail = default_output_message;

     opterr = 0;

     while ((c = getopt(argc, argv, "hve:")) != -1) {
	  switch (c) {
	  case 'h':
	       quit_msg(HELP);
	  case 'v':
	       quit_msg(VERSION);
	  case 'e':
	       print_string = optarg;
	       break;
	  case '?':
	       error_msg_detail[0] = optopt;	       error_msg_detail[1] = '\0';
	       if (optopt == 'e') {
		    quit_error_msg(OPTION_WITHOUT_ARGUMENT,
				   error_msg_detail);
	       } else {
		    quit_error_msg(OPTION_UNKNOWN, error_msg_detail);
	       }
	  default:
	       abort();
	  }
     }

     if (argc - optind != 1) {
	  quit_error_msg(TOO_MANY_ARGUMENTS, NULL);
     } else if (optind == argc) {
	  quit_error_msg(NO_OUTFILE_SPECIFIED, NULL);
     } else {
	  outfile_name = argv[optind];
     }

     outfile = fopen(outfile_name, "w");
     if (outfile == NULL) {
	  quit_error_msg(OUTFILE_OPEN_FAILURE, outfile_name);
     }

     fprintf(outfile, "%s\n", print_string);
     fclose(outfile);

     return(0);
}

void quit_msg(int message_code)
{
     printf("%s\n", message[message_code]);
     exit(0);
}

void quit_error_msg(int message_code, char detail_str[])
{
     fprintf(stderr, "%s %s\n", message[message_code], detail_str);
     exit(EXIT_FAILURE);
}
