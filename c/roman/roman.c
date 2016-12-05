/* roman.c -- Convert arabic to roman numerals
 * Andrew Cashner, 2016/12/04
 * Initial version, additive only (9 = VIIII, not IX)
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define MAX_STRING 80
#define MAX_RANGE  10000

const int max_roman_nums = 12;
const char *roman_str[] = {
     "I", "IV", "V", "IX",
     "X", "LX", "L", "XC",
     "C", "CD", "D", "CM",
     "M"
};
const int roman_int[] = {
     1,   4,   5,   9,
     10,  40,  50,  90,
     100, 400, 500, 900,
     1000
};


int to_roman(int arabic, char *roman);

int main(int argc, char *argv[])
{
     int arabic;
     char roman[MAX_STRING];
     
     if (argc != 2) {
	  fprintf(stderr, "Usage: roman INTEGER\n");
	  exit(EXIT_FAILURE);
     }
     if (sscanf(argv[1], "%d", &arabic) != 1) {
	  fprintf(stderr, "Bad argument %s.\n", argv[1]);
	  exit(EXIT_FAILURE);
     }
     
     if (to_roman(arabic, roman) == 0) {
	  printf("%s\n", roman);
     } else {
	  fprintf(stderr, "Input number out of range.\n");
	  exit(EXIT_FAILURE);
     }
	  
     return(0);
}

int to_roman(int arabic, char *roman)
{
     int i;
     roman[0] = '\0';

     if (arabic <= 0 || arabic >= MAX_RANGE) {
	  return(1);
     }

     i = max_roman_nums;
     while (arabic > 0) {
	  for (; i > 0; --i) {
	       if (arabic >= roman_int[i]) {
		    break;
	       }
	  }
	  strcat(roman, roman_str[i]);
	  arabic -= roman_int[i];
     }

     return(0);
}
