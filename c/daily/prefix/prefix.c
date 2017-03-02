#include<stdio.h>
#include<string.h>
int main(int argc, char *argv[])
{
  int i, prefix;
  char first[64], second[64], output[64];
  if (argc == 3) {
    strcpy(first, argv[1]);
    strcpy(second, argv[2]);
  } else return(-1);
  if (first[0] != second[0]) {
    return(-1);
  }
  for (i = prefix = 1; i < strlen(first) && i < strlen(second); ++i) {
    if (first[i] == second[i]) {
      ++prefix;
    }
  }
  for (i = 0; i < prefix; ++i) {
    output[i] = first[i];
  }
  output[i] = '\0';
  printf("%s\n", output);
  return(0);
}
    
    
