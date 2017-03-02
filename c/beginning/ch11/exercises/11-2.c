/* Andrew Cashner 2013-09-22 
* Oualline exercise 11-2, based on example 11-3
* Draw a 10-by-10 bitmapped square */

#include <stdio.h>

#define X_SIZE 10 /* size of array in X direction */
#define Y_SIZE 10 /* size of array in Y direction */

/*
* We use X_SIZE/8 because we pack 8 bits per byte
*/

char graphics[X_SIZE /8][Y_SIZE]; /* the graphics data */

#define SET_BIT(x,y) graphics[(x)/8][y] |= (0x80 >>((x)%8))

/************************************************************/

int main() 
{
	int x_index;			/* current x-axis location we are setting */
	int y_index;			/* y-axis */
	void print_graphics(void);	/* print the data */

	/* Draw top and bottom lines: 
	*  Fill in all bits of first and last rows with 1s */

	for (x_index = 0; x_index <= X_SIZE; ++x_index) 
	{
		SET_BIT(x_index, 0);		/* First row, y=0 */
		SET_BIT(x_index, Y_SIZE-1);	/* Last row, y=Y_SIZE-1 (why?) */
	}
	
	/* Draw vertical lines on sides:
	*  Fill in first and last bit of each row with 1s */

	for (y_index = 1; y_index < Y_SIZE; ++y_index)
	{
		SET_BIT(0, y_index);	/* First column, x=0; successive rows */
		SET_BIT(X_SIZE, y_index); /* Last column, x=X_SIZE */
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

			/* <= added to correct Oualline example, because an X_SIZE
			 * smaller than 16 will be 1, limiting horizontal range to 8
			 * columns */

		for (x = 0; x <= X_SIZE / 8; ++x)  
		{
			/* Handle each bit */
			for (bit = 0x80; bit > 0; bit = (bit >> 1)) 
			{
				if ((graphics[x][y] & bit) != 0) 
					printf("X");
				else
					printf(" ");
			}
		}
		printf("\n");
	}
}
