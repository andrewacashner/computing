/* argp-aac.c -- my attempt at argp */

#include <stdlib.h>
#include <argp.h>

const char *argp_program_version =
     "argp test 1.0";
const char *argp_program_bug_address =
     "<andrewacashner@gmail.com>";
static char *doc =
     "argp test -- command-line options and arguments with argp";
static char *args_doc = "FILE";

static struct argp_option options[] = {
     {"output", 'o', "FILE", 0, "Output to FILE instead of standard output" },
     { 0 }
};

struct arguments {
     char *args;
     char *output_file;
};

static error_t parse_opt(int key, char *arg, struct argp_state *state)
{
     struct arguments *arguments = state->input;

     switch (key) {
     case 'o':
	  arguments->output_file = arg;
	  break;
     case ARGP_KEY_ARG:
	  if (state->arg_num >= 1) {
	       argp_usage(state);
	  }
	  arguments->args = arg;
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
     arguments.output_file = "-";

     argp_parse(&argp, argc, argv, 0, 0, &arguments);

     printf("Input file (arg1) = %s\n"
	    "Output file = %s\n",
	    arguments.args[0], arguments.output_file);

     return(0);
}
	       
