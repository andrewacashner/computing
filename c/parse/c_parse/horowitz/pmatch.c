/* Knuth, Morris, Pratt string-matching algorith, from Horowitz p. 89 */
/* Andrew Cashner, 2015/10/28 */

#include <stdio.h>
#include <string.h>

#define MAX_STRING_SIZE 100
#define MAX_PATTERN_SIZE 100

int pmatch(char*,char*);
void fail(char*);

int failure[MAX_PATTERN_SIZE];
char string[MAX_STRING_SIZE];
char pat[MAX_PATTERN_SIZE];
 
int main(void)
{
  
  return(0);
}

int pmatch(char *string, char *pat)
{
  int i = 0, j = 0;
  int lens = strlen(string);
  int lenp = strlen(pat);
  int output;

  while (i < lens && j < lenp) {
    if (string[i] == pat[j]) {
      ++i, ++j;
    } else if (j == 0) {
      ++i;
    } else {
      j = failure[j - 1] + 1;
    }
  }
  if (j == lenp) {
    output = i - lenp;
  } else {
    output = -1;
  }
  
  return(output);
}

void fail(char *pat)
{
  /* Compute the pattern's failure function */
  int n = strlen(pat);
  failure[0] = -1;
  for (j = 1; j < n; ++j) {
    i = failure[j - 1];
    while ((pat[j] != pat[i + 1]) && (i >= 0)) {
      i = failure[i];
    }
    if (pat[j] == pat[i + 1]) {
      failure[j] = i + 1;
    } else {
      failure[j] = -1;
    }
  }
}

    
