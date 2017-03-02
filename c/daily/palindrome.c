/* Detect a palindrome */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define STR_MAX 148

int main(int argc, char *argv[])
{
  char msg[STR_MAX];
  char *result_msg[] = { "It is not a palindrome!\n", "It is a palindrome!\n" };
  int i, j;
  bool palindrome_bool = true;
  int lower_upper_diff = 'a' - 'A';

  if (argc != 2) {
    fprintf(stderr, "Wrong number of arguments.\n");
    exit(EXIT_FAILURE);
  }
  if (strlen(argv[1]) > STR_MAX) {
    fprintf(stderr, "Input string too long.\n");
    exit(EXIT_FAILURE);
  }
  strcpy(msg, argv[1]);
  for (i = 0; i < strlen(msg); ++i) {
    if (msg[i] >= 'a' && msg[i] <= 'z' ) {
      msg[i] -= lower_upper_diff;
    }
  }
  printf("%s\n", msg);
  for (i = 0, j = strlen(msg) - 1; i < j; ++i, --j) {
    if (msg[i] == ' ') ++i;
    if (msg[j] == ' ') --j;
    if (msg[i] != msg[j]) {
      palindrome_bool = false;
      break;
    }
  }
  printf("%s", result_msg[palindrome_bool]);
  return(0);
}
    
