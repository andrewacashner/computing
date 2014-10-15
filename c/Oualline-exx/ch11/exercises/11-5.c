/* Andrew Cashner 2013-10-04
*  Oualline ch 11 exercise 5
*  Write a program that takes a 32-bit integer (long int) and splits it into
*  eight 4-bit values. (Be careful of the sign bit.)
*/

# include <stdio.h>

int main ()
{
	
	char 		line [100];		/* User input buffer */
	long int 	start_int;		/* Starting 32-bit long integer */
	long int 	bit_test;		/* To test against bits of start_int */
	int 		loop_count;		/* Count repetitions */

	/* Get integer from user */
	printf 	("Split 32-bit integer into four-bit segments.\n");
	printf 	("Enter an integer: ");
	fgets 	(line, sizeof(line), stdin);
	sscanf 	(line, "%ld", &start_int);

	/* Loop through bits and print them in four-bit groups */

	for (loop_count = 0; loop_count < 32; ++loop_count) /* Max 32 bits */
	{
		/* Test one bit at a time, left to right */
		bit_test = 1 << ( 31 - loop_count );
		
		/* If bit is 1, print 1; else 0 */
		if ( (start_int & bit_test) > 0 ) 
			printf ("1");
		else printf ("0");
		
		/* Add a space after every 4 digits */
		if ( (loop_count + 1) % 4 == 0 ) 
			printf (" ");
	}
	
	printf ("\n");

	return (0);
}
