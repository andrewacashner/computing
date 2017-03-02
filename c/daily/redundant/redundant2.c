#include<stdio.h>

int ARRAY_MAX = 10;
void printarray(int*,int);

int main(void)
{
  int input_array[] = {4, 2, 3, 3, 13, 2, 42, 27, 4, 6};
  int i, j, total_unica, test, u;
  int unica[ARRAY_MAX];
  for (i = 0; i < ARRAY_MAX; ++i) {
    unica[i] = 0;
  }
  j = total_unica = 0;
  for (i = 0; i < ARRAY_MAX; ++i) {
    test = input_array[i];
    for (j = i + 1; j < ARRAY_MAX; ++j) {
      if (test == input_array[j]) {
	unica[total_unica] = test;
	++total_unica;
      }
    }
  }
  printf("[");
  printarray(input_array, ARRAY_MAX);
  printf("] => [");
  printarray(unica, total_unica);
  printf("]\n");

  return(0);
}

void printarray(int array[], int size)
{
  int i;
  for (i = 0; i < size; ++i) {
    printf("%d", array[i]);
    if (i < size - 1) {
      printf(", ");
    }
  }
  return;
}

