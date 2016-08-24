#include <stdio.h>
int main(void)
{
  int alphabet[6][5];
  int row = 0;
  int column = 0;
  int c = 'A';

 loop1:
  column = 0;
 loop2:
  if (c <= 'Z') {
    alphabet[row][column] = c;
    ++c;
  } else {
    alphabet[row][column] = 0;
  }
  ++column;
  if (column < 5) goto loop2;
    
  ++row;
  if (row < 6) goto loop1;
     
  for (row = 0; row < 6; ++row) {
    for (column = 0; column < 5; ++column) {
      c = alphabet[row][column];
      putchar(c);
    }
  }

  puts("");
  return(0);
}
