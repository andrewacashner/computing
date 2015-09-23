/* average.c -- Andrew A. Cashner, 2015-09-22 
 * Average numbers from the command line with floating point 
 * arithmetic: Convert each argument to a double (exit with 
 * error if unsuccessful) and add it to running sum and add 
 * to tally of total items; then divide sum by total items 
 * and print the result.
 */

#include <stdio.h>
#include <stdlib.h>

int main (int argc, char *argv[])
{
  double current_addend, sum, average;
  int current_arg, sscanf_return, total_items;

  sum = total_items = 0;
  
  for (current_arg = argc - 1; current_arg > 0; --current_arg) {
    sscanf_return = sscanf(argv[current_arg], "%lf", &current_addend);

    if (sscanf_return != 1) {
      fprintf(stderr, "Bad input '%s' at argument %d.\n",
              argv[current_arg], current_arg);
      exit(EXIT_FAILURE);
    }

    sum += current_addend;
    ++total_items;
  }

  average = sum / total_items;
  printf("%g\n", average);

  return(0);
}
