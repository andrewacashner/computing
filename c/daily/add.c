/* add.c -- Andrew A. Cashner, 2015-03-09
 * Add numbers from the command line with floating point arithmetic:
 * Convert each argument to a double (exit with error if unsuccessful) and add it
 * to running sum, then print sum.
 */

#include <stdio.h>
#include <stdlib.h>

int main (int argc, char *argv[])
{
	double current_addend, sum;
	int current_arg, sscanf_return;
	
	for (current_arg = argc - 1, sum = 0.0; current_arg > 0; --current_arg) {
		sscanf_return = sscanf(argv[current_arg], "%lf", &current_addend);

		if (sscanf_return != 1) {
			fprintf(stderr, "Bad input '%s' at argument %d.\n",
				argv[current_arg], current_arg);
			exit(EXIT_FAILURE);
		}

		sum += current_addend;
	}
	printf("%f\n", sum);

	return(0);
}
