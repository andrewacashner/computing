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

#define MAX_STRING 80*100

char default_output_message[] =
     "I think the most important thing is that I have faith,\n"
     "and that I have tried to express that faith through my music.\n"
     "That is all that matters.  --Olivier Messiaen";
char default_outfile_name[] = "getopt-aac-output.txt";

const enum {
     HELP,
     VERSION,
     TOO_MANY_ARGUMENTS,
     NO_OUTFILE_SPECIFIED,
     OUTFILE_OPEN_FAILURE
} message_codes;

const char *message[] = {
     "Help message.",
     "Version message.",
     "Too many arguments.",
     "No output file specified.",
     "Could not open file %s for writing."
};

/* FUNCTION PROTOTYPES */
void quit_msg(int message_code);
void quit_error_msg(int message_code, char detail_msg[]);


int main(int argc, char *argv[])
{
     int c;
     FILE *outfile;
     char *outfile_name = default_outfile_name;
     char *print_string = default_output_message;

     while ((c = getopt(argc, argv, "hve:")) != -1) {
	  switch (c) {
	  case 'h':
	       quit_msg(HELP);
	  case 'v':
	       quit_msg(VERSION);
	  case 'e':
	       print_string = optarg;
	       break;
	  default:
	       exit(EXIT_FAILURE);
	  }
     }

     if (argc - optind > 1) {
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

void quit_error_msg(int message_code, char detail_msg[])
{
     if (detail_msg != NULL) {
	  fprintf(stderr, "%s %s\n", message[message_code], detail_msg);
     } else {
	  fprintf(stderr, "%s\n", message[message_code]);
     }
     exit(EXIT_FAILURE);
}
