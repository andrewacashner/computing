/* roman.c -- Convert arabic to roman numerals
 * Andrew Cashner, 2016/12/04
 * Initial version, additive only (9 = VIIII, not IX)
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define MAX_STRING 80
#define MAX_RANGE  10000

const char roman_char[] = "IVXLCDM";
const int roman_int[] = {1, 5, 10, 50, 100, 500, 1000};
const int max_roman_chars = 6;

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
     int i, j;
     int roman_index = 0;
     bool subtract = false;

     if (arabic <= 0 || arabic >= MAX_RANGE) {
	  return(1);
     }

     i = max_roman_chars;
     while (arabic > 0) {
	  for (; i > 0; --i) {
	       if (arabic >= roman_int[i]) {
		    break;
	       }
	  }

	  
	  if (arabic != roman_int[i]) {
	       j = i + 1;
	       if (arabic == roman_int[j] - roman_int[i]) {
		    subtract = true;
	       } else if (arabic == roman_int[j] - roman_int[i - 1]) {
		    --i;
		    subtract = true;
	       }
	  }
	
	  if (subtract == true) {
	       roman[roman_index++] = roman_char[i];
	       roman[roman_index++] = roman_char[j];
	       subtract = false;
	       arabic -= roman_int[j] - roman_int[i];
	  } else {
	       roman[roman_index++] = roman_char[i];
	       arabic -= roman_int[i];
	  }
     }

     roman[roman_index] = '\0';

     return(0);
}
