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

const char output_message[] =
     "I think the most important thing is that I have faith,\n"
     "and that I have tried to express that faith through my music.\n"
     "That is all that matters.  --Olivier Messiaen\n";
     
int main(int argc, char *argv[])
{
     int c, index;
     bool hflag, vflag, eflag;
     opterr = 0;

     while ((c = getopt(argc, argc, "hve:")) != -1) {
	  switch (c) {
	  case 'h':
	       hflag = true;
	       break;
	  case 'v':
	       vflag = true;
	       break;
	  case 'e':
	       eflag = true;
	       break;
	  case '?':
	       /* TODO error case */
	  default:
	       abort();
	  }
     
     return(0);
}
