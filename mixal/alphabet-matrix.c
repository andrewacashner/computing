#include <stdio.h>
int main(void)
{
  int alphabet[6][5];
  int row, column;
  int c = 'A';

  for (row = 0; row < 6; ++row) {
    for (column = 0; column < 5; ++column) {
      if (c <= 'Z') {
	alphabet[row][column] = c;
	++c;
      } else {
	alphabet[row][column] = 0;
      }
    }
  }
     
  for (row = 0; row < 6; ++row) {
    for (column = 0; column < 5; ++column) {
      c = alphabet[row][column];
      putchar(c);
    }
  }

  puts("");
  return(0);
}
