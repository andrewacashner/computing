/* argp.c -- test of argp from GNU C library
 * Andrew Cashner, 2016/12/01
 */

#include <stdlib.h>
#include <argp.h>

const char *argp_program_version     =
     "argp version 0.0 for testing only";
const char *argp_program_bug_address =
     "<author@example.com>";
static char doc[] =
     "argp example #2 -- a minimal program using argp";

static struct argp argp = { 0, 0, 0, doc };

int main(int argc, char *argv[])
{
     argp_parse(&argp, argc, argv, 0, 0, 0);
     return(0);
}
