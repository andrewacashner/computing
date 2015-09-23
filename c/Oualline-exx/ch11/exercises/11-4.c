/* Andrew Cashner 2013-09-29
* Oualline exercise 11-4
* Write a program that counts the number of bits set in an integer. For
* example, the number 5 (decimal), which 0000000000000101 (binary), has two
* bits set. */

# include <stdio.h>

int main ()
{
	char line [100]; 	/* User input buffer */
	int input_int;		/* Number to test  */
	int loop_count;		/* Counter for loop */
	int bit_test;		/* Variable to compare with input_int */
	int test_result;  	/* Result of comparison */
	int bit_count;		/* Number of bits set in the integer */

/* Ask for number */

	printf ("Count number of bits in an integer.\n");
	printf ("Enter an integer: ");
	fgets (line, sizeof (line), stdin);
	sscanf (line, "%d", &input_int);

	/* Debug 
	printf ("Decimal %d = Hexadecimal %x\n", input_int, input_int); 
	*/

/* Index through each bit, use boolean to increment counter */
	
	bit_count = 0;
	bit_test = 0;

	for (loop_count = 0; loop_count < 16; ++loop_count) /* Max 16 bits */
	{
		bit_test = (1 << loop_count);
		test_result = input_int & bit_test;
	
		if ( test_result > 0 )	
			++bit_count;

		/* Debug 
		printf("bit_test = %x\n", bit_test);
		printf("test_result = %d\n", test_result);
		printf("bit_count after test = %d\n", bit_count);
		*/
	}

/* Print results */

	printf ("There are %d bits set in the integer %d.\n", 
			bit_count, input_int);

	return (0);
}
