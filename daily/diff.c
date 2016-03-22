#include<stdio.h>
#include<stdlib.h>

#define SWAP(x,y,t) ((t) = (x), (x) = (y), (y) = (t))

void sort(int[], int);

int main(int argc, char *argv[])
{
  int nums[3];
  int i, test;

  if (argc != 4) {
    fprintf(stderr, "Incorrect number of arguments.\n 'diff' takes three integer arguments, as in 'diff 1 2 3'\n");
    exit(EXIT_FAILURE);
  }
  for (i = 0; i < 3; ++i) {
    test = sscanf(argv[i + 1], "%d", &nums[i]);
    if (test != 1) {
      fprintf(stderr, "Error: Non-integer input '%s'.\n", argv[i + 1]);
      exit(EXIT_FAILURE);
    }
  }
  printf("Input: (%d, %d, %d)\n\n", nums[0], nums[1], nums[2]);

  sort(nums,3);
  printf("Sorted: (%d, %d, %d)\n\n", nums[0], nums[1], nums[2]);
  
  if ((nums[0] - nums[1] == nums[2]) || (nums[1] - nums[0] == nums[2])) {
    printf("True\n");
  } else printf("False\n");
    
  return(0);
}

void sort(int list[], int n)
{
  int i, j, max, tmp;
  for (i = 0; i < n - 1; ++i) {
    max = i;
    for (j = i + 1; j < n; ++j) {
      if (list[j] > list[max]) {
	max = j;
      }
    }
    SWAP(list[i], list[max], tmp);
  }
  return;
}


    
