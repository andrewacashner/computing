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
void read_short_option(char option_input, char parameter[]);
void read_long_option(char option_input[], char parameter[]);
void read_main_arg(char arg_input[]);
void do_option(int option_code, char parameter[]);

/* Global variables */
int global_frequency = 1000;
int global_rate = 20;
char global_filename[80];

/* Constants for lookup */
const char *option_string[] = {
     "help", "version", "frequency", "rate"
};
const enum {
     OPT_HELP, OPT_VERSION, OPT_FREQUENCY, OPT_RATE,
     OPT_UNKNOWN
} option_code;


int main(int argc, char *argv[])
{
     process_options(argc, argv);

     printf("Frequency = %d\n"
	    "Rate      = %d\n"
	    "Filename  = '%s'\n",
	    frequency, rate, filename);

     return(0);
}

void process_options(int argc, char *argv[])
{
     int i;

     for (i = 1; i < argc; ++i) {
	  if (argv[i][0] == '-') {
	       if (argv[i][1] == '-') {
		    read_long_option(&argv[i][2], argv[i + 1]);
		    ++i;
	       } else if (argv[i][2] == '\0') {
		    read_short_option(argv[i][1], argv[i + 1]);
		    ++i;
	       } else {
		    do_option(OPT_UNKNOWN, "");
	       }
	  } else {
	       read_main_arg(argv[i]);
	  }
     }

     return;
}


void read_short_option(char option_input, char parameter[])
{
     return;
}

void read_long_option(char option_input[], char parameter[])
{
     return;
}

void read_main_arg(char arg_input[])
{
     strcpy(global_filename, arg_input);
     return;
}

void do_option(int option_code, char parameter[])
{
     
     return;
}

