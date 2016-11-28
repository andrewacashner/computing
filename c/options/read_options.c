/* read_options.c -- Read command line options
 * Andrew A. Cashner, 2016/11/27
 * 
 * Usage: read_options [-s <#> --long <#>] filename.ext
 *
 * Possible options:
 * Short  Long         Parameter   Action
 * -h     --help       <none>      Print help message and exit
 * -v     --version    <none>      Print version message and exit
 * -f     --frequency  <int>       Set frequency to <integer>
 * -r     --rate       <int>       Set rate to <integer>
 * 
 * The program either prints a help or version message;
 * or prints a frequency, rate, and filename;
 * using default values for frequency and rate or values set by
 * options.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

/* Function prototypes */
void process_options(int argc, char *argv[]);
void read_option(int option_type, char option_input[], char parameter[]);
void read_main_arg(char arg_input[]);
void do_option(int option_code, char option_input[], char parameter[]);


/* Constants for lookup */
const enum { SHORT_OPTION, LONG_OPTION } option_type;

const char *option_string[] = {
     "help", "version", "frequency", "rate"
};
const enum {
     OPT_HELP,
     OPT_VERSION,
     OPT_FREQUENCY,
     OPT_RATE,
     OPT_MISSING_PARAMETER,
     OPT_UNKNOWN
} option_code;

/* Error messages */
const char *typeout_msg[] = {
     "Help info",
     "Version number",
     "Bad option",
     "Bad parameter",
     "Missing parameter for option",
     "Unknown error"
};
const enum {
     MSG_HELP,
     MSG_VERSION,
     MSG_BAD_OPTION,
     MSG_BAD_PARAMETER,
     MSG_MISSING_PARAMETER,
     MSG_UNKNOWN_ERROR
} typeout_msg_code;


/* Global variables */
int global_frequency = 1000;
int global_rate = 20;
char global_filename[80];

int main(int argc, char *argv[])
{
     process_options(argc, argv);

     printf("Frequency = %d\n"
	    "Rate      = %d\n"
	    "Filename  = '%s'\n",
	    global_frequency, global_rate, global_filename);

     return(0);
}

void process_options(int argc, char *argv[])
{
     int i;

     for (i = 1; i < argc; ++i) {
	  if (argv[i][0] == '-') {
	       if (argv[i][1] == '-') {
		    read_option( (int)LONG_OPTION, &argv[i][2], argv[i + 1]);
		    ++i;
	       } else if (argv[i][2] == '\0') {
		    read_option( (int)SHORT_OPTION, &argv[i][1], argv[i + 1]);
		    ++i;
	       } else do_option(OPT_UNKNOWN, argv[1], NULL);
	  } else read_main_arg(argv[i]);
     }
     return;
}


void read_option(int option_type, char option_input[], char parameter[])
{
     int i;
     for (i = 0; i < OPT_UNKNOWN; ++i) {
	  if (option_input[0] == option_string[i][0]) {
	       if (option_type == LONG_OPTION) {
		    if (strcmp(option_input, option_string[i]) == 0) {
			 break;
		    }
	       } else break;
	  }
     }
     do_option(i, option_input, parameter);
     return;
}

void read_main_arg(char arg_input[])
{
     strcpy(global_filename, arg_input);
     return;
}

void do_option(int option_code, char option_input[], char parameter[])
{
     switch (option_code) {
     case OPT_HELP:
	  printf("%s\n", typeout_msg[MSG_HELP]);
	  exit(0);
     case OPT_VERSION:
	  printf("%s\n", typeout_msg[MSG_VERSION]);
	  exit(0);
     case OPT_FREQUENCY:
	  if (parameter == NULL) {
	       do_option(OPT_MISSING_PARAMETER, option_input, parameter);
	  } else if (sscanf(parameter, "%d", &global_frequency) != 1) {
	       fprintf(stderr, "%s '%s'\n", typeout_msg[MSG_BAD_PARAMETER], parameter);
	       exit(EXIT_FAILURE);
	  } else break;
     case OPT_RATE:
	  if (parameter == NULL) {
	       do_option(OPT_MISSING_PARAMETER, option_input, parameter);
	  } else if (sscanf(parameter, "%d", &global_rate) != 1) {
	       fprintf(stderr, "%s '%s'\n", typeout_msg[MSG_BAD_PARAMETER], parameter);
	       exit(EXIT_FAILURE);
	  } else break;
     case OPT_UNKNOWN:
	  fprintf(stderr, "%s '%s'\n", typeout_msg[MSG_BAD_OPTION], option_input);
	  exit(EXIT_FAILURE);
     case OPT_MISSING_PARAMETER:
	  fprintf(stderr, "%s '%s'\n", typeout_msg[MSG_MISSING_PARAMETER], option_input);
	  exit(EXIT_FAILURE);
     default:
	  fprintf(stderr, "%s\n", typeout_msg[MSG_UNKNOWN_ERROR]);
	  exit(EXIT_FAILURE);
     }
     
     return;
}

