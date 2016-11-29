/* getopt-tutorial.c -- Andrew Cashner, 2016/11/29
 * Learning to use 'getopt'
 * From https://www.gnu.org/software/libc/manual/html_node/
 *        Using-Getopt.html
 */


#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <ctype.h>
#include <unistd.h>
#include <getopt.h>

int main(int argc, char *argv[])
{
     int c, index;
     bool hflag, vflag;
     char *ovalue = NULL;

     opterr = 0;

     while ((c = getopt(argc, argv, "hvo:")) != -1) {
	  switch (c) {
	  case 'h':
	       hflag = true;
	       break;
	  case 'v':
	       vflag = true;
	       break;
	  case 'o':
	       ovalue = optarg;
	       break;
	  case '?':
	       if (optopt == 'o') {
		    fprintf(stderr, "Option -%o requires an argument.\n", optopt);
	       } else if (isprint(optopt)) {
		    fprintf(stderr, "Unknown option '-%c'.\n", optopt);
	       } else {
		    fprintf(stderr, "Unknown option character '\\x%x'.\n", optopt);
	       }
	       return(1);
	  default:
	       abort();
	  }
     }

     printf("hflag = %d, vflag = %d, ovalue = %s\n",
	    hflag, vflag, ovalue);

     for (index = optind; index < argc; ++index) {
	  printf("Non-option argument %s\n", argv[index]);
     }
     
     return(0);
}
