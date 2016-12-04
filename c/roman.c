/* roman numerals
 * Andrew Cashner, 2016/12/3
 */

#include <stdio.h>
#include <stdlib.h>

#define INCREMENT_MAX 7

const int increment_value[] = { 1, 5, 10, 50, 100, 500, 1000 };
const char increment_char[] = "IVXLCDM";

void roman_numeral(int arabic);
void exit_usage(void);

int main(int argc, char *argv[])
{
     int arabic;
     if (argc != 2) {
	  exit_usage();
     }
     if (sscanf(argv[1], "%d", &arabic) != 1) {
	  exit_usage();
     }
     
     roman_numeral(arabic);
	  
     return(0);
}

void roman_numeral(int arabic)
{
     int i, diff;
     if (arabic < 1) {
	  printf("Nullus\n");
	  return;
     }
     for (i = 0; i < INCREMENT_MAX - 1; ++i) {
	  if (arabic >= increment_value[i]
	      && arabic < increment_value[i + 1]) {
	       printf("Between %c and %c\n",
		       increment_char[i], increment_char[i + 1]);
	       break;
	  }
     }
     if (arabic - increment_value[i + 1] < 0) {
	  printf("%c%c", increment_char[i], increment_char[i + 1]);
     } else {
	  printf("%c", increment_char[i]);
	  diff = arabic - increment_value[i];
	  for ( ; diff > 0 ; --diff) {
	       printf("%c", increment_char[i - 1]);
	  }
     }

     printf("\n");
     return;
}

void exit_usage(void)
{
	  fprintf(stderr, "Usage: roman NUMBER\n");
	  exit(EXIT_FAILURE);
}
