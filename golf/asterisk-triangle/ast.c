#include <stdio.h>
int main(int argc, char *argv[])
{
  int i, j, k;
  if (argc != 2) {
    return(1);
  }
  sscanf(argv[1], "%d", &i);
  for (j = 0; j < i; ++j) {
    for (k = 0; k <= j; ++k) {
      printf("*");
    }
    printf("\n");
  }

  return(0);
}
