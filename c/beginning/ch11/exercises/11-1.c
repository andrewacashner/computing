/* Oualline exercise 11-1: Write a set of parameterized macros, clear_bit
 * and test_bit, to go with the set_bit operation defined in example
 * 11-3. Write a main program to test these macros. */

#include <stdio.h>

#define X_SIZE 40 /* size of array in X direction */
#define Y_SIZE 40 /* size of array in Y direction */

char graphics[X_SIZE / 8][Y_SIZE]; /* the graphics data */

#define SET_BIT(x,y) graphics[(x)/8][y] |= (0x80 >> ((x)%8))

#define TEST_BIT(x,y) (graphics[(x)/8][y] & 1)

#define CLEAR_BIT(x,y) graphics[(x)/8][y] &= ~graphics[(x)/8][y]

/****************************************************/

int main()
{
	int loc; 	/* current location we are setting */

	for (loc = 0; loc < X_SIZE; ++loc)
	{
		SET_BIT(loc, loc);

		/* Test the bits */
		printf("Bit[%d][%d] = %d\n", loc, loc, TEST_BIT(loc, loc));

	}

	print_graphics();
	
	/* Clear the bits */

	for (loc = 0; loc < X_SIZE; ++loc)
	{
		CLEAR_BIT(loc, loc);

		/* Test the bits */
		printf("Bit[%d][%d] = %d\n", loc, loc, TEST_BIT(loc, loc));
		
	}

	print_graphics();

	return(0);
}

/************************************************************
* print_graphics -- Prints the graphics bit array as a set
* of X and .'s.
************************************************************/

void print_graphics(void) 
{
	int x;				/* current x BYTE */
	int y;				/* current y location */
	unsigned int bit;	/* bit we are testing in the current byte */

	for (y = 0; y < Y_SIZE; ++y) 
	{
		/* Loop for each byte in the array */
		for (x = 0; x < X_SIZE / 8; ++x) 
		{
			/* Handle each bit */
			for (bit = 0x80; bit > 0; bit = (bit >> 1)) 
			{
				if ((graphics[x][y] & bit) != 0) 
					printf("X");
				else
					printf(".");
			}
		}
		printf("\n");
	}
}
