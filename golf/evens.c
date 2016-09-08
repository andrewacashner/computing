#include<stdio.h>
#include<stdbool.h>
int main(void)
{
  bool test = false;
  int c;
  int diff = 'a' - 'A';
  while ((c = getchar()) != '\n') {
    if (test == true) {
      if (c > 'Z') {
	c -= diff;
      }
      test = false;
    } else {
      if (c < 'a') {
	c += diff;
      }
      test = true;
    }
    putchar(c);
  }
  printf("\n");
  return(0);
}
