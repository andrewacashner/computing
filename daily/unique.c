#include<stdio.h>
#include<stdbool.h>

int main(void)
{
  int i, c;
  int ascii[128];
  for (i = 0; i < 128; ++i) {
    ascii[i] = false;
  }
  while ((c = getchar()) != '\n') {
    if (ascii[c] == false) {
      ascii[c] = true;
      putchar(c);
    }
  }
  puts("\n");
  return(0);
}

  
