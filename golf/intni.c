/* Find smallest integer divisible by input that is a palindrome */
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#define MAX 10000000

int main(int argc, char *argv[])
{
  int input, length, i, j;
  unsigned long int current, result;
  bool palindrome = false;
  char test[8];
  sscanf(argv[1], "%d", &input);

  if (input >= 0 && input < 10) {
    result = input;
  } else if (input % 10 == 0) {
    printf("No answer found.\n");
    return(0);
  } else {
    for (current = input; current < MAX; current += input) {
      sprintf(test, "%ld", current);
      length = strlen(test) - 1;
      for (i = 0, j = length; i < j; ++i, --j) {
	if (test[i] != test[j]) {
	  palindrome = false;
	  break;
	} else {
	  palindrome = true;
	}
      }
      if (palindrome == true) {
	result = current;
	break;
      }
    } 
  }

  printf("%ld\n", result);
  
  return(0);
}
