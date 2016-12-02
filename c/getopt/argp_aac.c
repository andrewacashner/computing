/* argp-aac.c -- my attempt at argp */

#include <stdlib.h>
#include <argp.h>
#include <stdbool.h>

const char *argp_program_version =
     "argp test 1.0";
const char *argp_program_bug_address =
     "<andrewacashner@gmail.com>";
static char doc[] =
     "argp test -- command-line options and arguments with argp";
static char args_doc[] =
     "INFILE OUTFILE";
static struct argp_option options[] = {
     {"format", 'f', "FORMAT", 0, "Specify output format (default: .txt)" },
     { 0 }
};

struct arguments {
     char *args[2];
     bool format_mode;
     char *format_string;
};

static error_t parse_opt(int key, char *arg, struct argp_state *state)
{
     struct arguments *arguments = state->input;

     switch (key) {
     case 'f':
	  arguments->format_string = arg;
	  arguments->format_mode = true;
	  break;
     case ARGP_KEY_ARG:
	  if (state->arg_num >= 2) {
	       argp_usage(state);
	  }
	  arguments->args[state->arg_num] = arg;
	  break;
     case ARGP_KEY_END:
	  if (state->arg_num < 2) {
	       argp_usage(state);
	  }
     default:
	  return(ARGP_ERR_UNKNOWN);
     }
     return(0);
}

static struct argp argp = { options, parse_opt, args_doc, doc };

int main(int argc, char *argv[])
{
     struct arguments arguments;
     char *infile_name, *outfile_name;
     arguments.format_string = "txt";
     
     argp_parse(&argp, argc, argv, 0, 0, &arguments);

     infile_name = arguments.args[0];
     outfile_name = arguments.args[1];

     if (arguments.format_mode == true) {
	  printf("Using file format %s...\n", arguments.format_string);
     }
     printf("Input file  = %s\n"
	    "Output file = %s\n",
	    infile_name, outfile_name);

     return(0);
}
	       
