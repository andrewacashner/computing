/* Andrew Cashner 2013-10-04
* Oualline exercise 11-6
* Write a program that will take the bits in a number and shift them to the left
* end. For example, 01010110 (binary) would become (11110000) (binary). */

# include <stdio.h>

/* Get integer from user input */

int get_integer ()			
{
	char 	line [100];		/* User input buffer */
	int		user_int;		/* Integer input by user */

	printf 	("Enter an integer between 0 and 65535: ");
	fgets 	(line, sizeof (line), stdin);
	sscanf 	(line, "%d", &user_int);

	return (user_int);
}

/* Count number of bits set in an integer (16-bit) */
int bit_count (int start_int)	
{
	int bit_test;			/* Integer to test bits against input integer */
	int loop_count;			/* Counter for loop */
	int bit_counter = 0;	/* Number of bits set in the integer */

	/* Index through each bit, use boolean to increment counter */
	for (loop_count = 0; loop_count < 16; ++loop_count) /* Max 16 bits */
	{
		bit_test = 1 << ( 15 - loop_count);	/* Test each bit left to right */
	
		if ( ( start_int & bit_test ) > 0 )	
			++bit_counter;
	}

	return (bit_counter);
}

/* Print the last 16 bits of an integer in binary */
void print_binary (int input_int)
{
	int 	bit_test;		/* To test against bits of start_int */
	int 	loop_count;		/* Count repetitions */

	/* Loop through bits and print them as 1s and 0s */
	for (loop_count = 0; loop_count < 16; ++loop_count) /* Max 16 bits */
	{
		/* Test one bit at a time, left to right */
		bit_test = 1 << ( 15 - loop_count );
		
		/* If bit is 1, print 1; else 0 */
		if ( (input_int & bit_test) > 0 ) 
			printf ("1");
		else printf ("0");
		
		/* Add a space after every 4 digits */
		if ( (loop_count + 1) % 4 == 0 ) 
			printf (" ");
	}
	
	printf ("\n");

	return;
}

/****************************************************************************/

int main ()
{
	int		start_int;	/* Integer input by user */
	int		end_int;	/* Integer with 1s shifted to left */
	int		loop_count;	/* Number of repetitions */ 

	start_int = get_integer ();	/* Get integer from user */

	print_binary (start_int);	/* Print binary of user-input integer */
	
	/* Create new integer with all bits from input integer moved to left */
	
	end_int = 0;			/* Start with all 0s */
	
	/* Move from left to right and set bits as 1; set as many bits as there were
	 * in the original user input integer, determined by bit_count */

	for (loop_count = 0; loop_count < (bit_count (start_int)); ++loop_count)
	{
		/* Start setting bits at far left, bit position 15, move right each
		 * loop, then set the bit in each position */
		
		end_int |= (1 << (15 - loop_count) ); 
	}
	
	/* Print binary of result integer */
	print_binary (end_int);	

	return (0);
}
