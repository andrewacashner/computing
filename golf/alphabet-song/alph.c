#include<stdio.h>
int main(void)
{
  char *endchar = "GPSVZ";
  char c;
  char *endstring[] = { ", ", ",\n", "\n" };
  enum { COMMA, RETURN, ENDFILE } ending;
  int i, j;
  i = j = 0;
  for (c = 'A'; c <= 'Z'; ++c) {
    if (c == endchar[i]) {
      if (i < 4) {
	j = RETURN;
	++i;
      } else {
	j = ENDFILE;
      }
    } else {
      j = COMMA;
    }
    printf("%c%s", c, endstring[j]);
  }
  return 0;
}
