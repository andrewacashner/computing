/* getopt-long-aac.c -- my attempt at a simple option setup with long
 * options, 2016/11/29
 */

#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include <stdbool.h>

/* Constants */
const int default_frequency = 1000;
const int default_rate = 100;

/* Look-up table of error messages */
const char *message[] = {
     "This is the help message.",
     "This is the version message.",
     "This is the error message."
};
const enum { MSG_HELP, MSG_VERSION, MSG_ERROR } msg_code;


/* Function prototypes */
void typeout(int msg_code);


int main(int argc, char *argv[])
{
     int c;
     int option_index = 0;
     static struct option long_options[] = {
	  {"help",        no_argument,       0, 'h'},
	  {"version",     no_argument,       0, 'v'},
	  {"frequency",   required_argument, 0, 'f'},
	  {"rate",        required_argument, 0, 'r'},
	  {"output-file", required_argument, 0, 'o'},
	  {0, 0, 0, 0}
     };
     int frequency = default_frequency;
     int rate = default_rate;
     char *infile_name = NULL;
     char *outfile_name = NULL;
     bool outfile_specified = false;
	  
     while ((c = getopt_long(argc, argv, "hvf:r:o:",
			    long_options, &option_index)) != -1) {
	  switch (c) {
	  case 'h':
	       typeout((int)MSG_HELP);
	       exit(0);
	  case 'v':
	       typeout((int)MSG_VERSION);
	       exit(0);
	  case 'f':
	       if (sscanf(optarg, "%d", &frequency) != 1) {
		    fprintf(stderr, "Bad frequency argument %s\n", optarg);
		    exit(EXIT_FAILURE);
	       } else break;
	  case 'r':
	       if (sscanf(optarg, "%d", &rate) != 1) {
		    fprintf(stderr, "Bad frequency argument %s\n", optarg);
		    exit(EXIT_FAILURE);
	       } else break;
	  case 'o':
	       outfile_name = optarg;
	       outfile_specified = true;
	       break;
	  case '?':
	       /* getopt_long already displayed error message */
	       exit(EXIT_FAILURE);
	  default:
	       abort();
	  }
     }
     
     if (optind < argc) {
	  if (argc - optind > 1) {
	       fprintf(stderr, "Too many arguments.\n");
	       typeout((int)MSG_HELP);
	       exit(EXIT_FAILURE);
	  } else {
	       infile_name = argv[optind];
	  }
     } else {
	  fprintf(stderr, "No input file specified.\n");
	  typeout((int)MSG_HELP);
	  exit(EXIT_FAILURE);
     }
     
     if (outfile_specified == false) {
	  outfile_name = infile_name;
     }

     printf("Frequency:   %d\n"
	    "Rate:        %d\n"
	    "Output file: %s\n"
	    "Input file:  %s\n",
	    frequency, rate, outfile_name, infile_name);
     
     return(0);
}

void typeout(int msg_code)
{
     printf("%s\n", message[msg_code]);
     return;
}


