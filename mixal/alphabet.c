#include<stdio.h>
#define MAXROWS 6
#define MAXFIELDS 5
int main(void)
{
  int letter, row, field;
  char msg[MAXROWS][MAXFIELDS];
  row = 0;
  for (letter = 'A'; letter <= 'Z'; ++letter) {
    for (field = 0; field < MAXFIELDS; ++field) {
      msg[row][field] = (char)letter;
      printf("%c", msg[row][field]);
    }
    ++row;
  }
  for (row = 0; row < MAXROWS; ++row) {
    for (field = 0; field < MAXFIELDS; ++field) {
      printf("%c", msg[row][field]);
    }
  }
  printf("\n");
					
  return(0);
}
