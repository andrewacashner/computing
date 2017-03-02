#include<stdio.h>
int main(void)
{
  int i, c, buf[256];
  for (i = 0; ((buf[i] = getchar()) != EOF); ++i) {
    ;
  }
  for ( ; i >= 0; --i) {
    putchar(buf[i]);
  }
  return(0);
}
  
