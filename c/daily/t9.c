#define INPUT "8#44#33#0#999#*77#88#444#222#55#0#22#777#666#9#66#0#333#666#99#0#5#88#6#7#7777#0#666#888#33#777#0#8#44#33#0#555#2#99#*9999#999#0#3#666#4#"

#include<stdio.h>

static const char keyboard[10][4] = {" ", ".?!", "ABC", "DEF", "GHI", "JKL", "MNO", "PQRS", "TUV", "WXYZ"};

int main(void)
{
  char input[] = INPUT;
  char output[256];
  int i, j;
  int key = 0;
  int reps = 0;

  for (i = j = 0; input[i] != '\0'; ++i) {
    switch (input[i]) {
    case '#':
      output[j] = keyboard[key][reps - 1];
      ++j;
      reps = key = 0;
      break;
    case '*':
      if (j > 0) --j;
      break;
    default:
      if (reps == 0)  {
	key = (int)input[i] - '0';
      }
      ++reps;
      break;
    }
  }

  output[j] = '\0';
  printf("%s\n", output);
  
  return(0);
}
