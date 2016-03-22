/****************************************************
 * CDOKU : Generates sudoku tables.					*
 *												 	*
 * Andrew Cashner, 2014-01-01						*
 ***************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int rand_lim(int limit) {
	/* return a random number between 0 and limit inclusive */

	int divisor = RAND_MAX/(limit+1);
	int return_value;

	do {
		return_value = rand() / divisor;
	} 	while (return_value > limit); 

	return (return_value);
}

/***************************************************************************/

int main()
{
	int table[9][9] = {{0}};		/* 9x9 sudoku grid: Start with all zeros */
	int boxvalue = 0;				/* Value for each square */
	int x;							/* X coordinate */
	int y;							/* Y coordinate */
	int x2;							/* X coord. of square to compare with x */ 
	int y2;							/* Y coord. of square to compare with y */ 
	int failures;					/* Count number of failed comparisons */
	int success;					/* Test comparison of x,y with x2,y2: 
										0 for success; 1 for failure */ 
	int xcount;						/* Loop counter for indexing through 3x3 block: x values */
	int ycount; 					/* Loop counter for y values in block */

	/* Seed random-number generator */
	srand(time(NULL));				

	/* Index through every square in matrix and set values */
	for (y = 0; y < 9; ++y) {

	/* DEBUG  printf("Index: y = %d; ", y);
	*/

		for (x = 0; x < 9; ++x) {
	
			/* DEBUG  printf("Index: x = %d; ", x);
					printf("Square (x,y) (%d, %d)\n", x, y);
			*/		

			/* Pick random number and test it; repeat until successful value is
			 * found */
			

			for (success = 1; success != 0; ) { 

				failures = 0;

				/* Generate random number 1-9 */
				/* boxvalue = rand_lim(8) + 1; */

				/* DEBUG: dummy value generator */
				 ++boxvalue;
				if (boxvalue > 9)
					boxvalue = 0;


				/* DEBUG  printf("Random boxvalue = %d\n", boxvalue);
				*/
				

			/* Test number in particular square */
			/* Test y values: Scan column */

				/* DEBUG  printf("Testing y values\n");
				*/

				x2 = x;

				for (y2 = 0; y2 < 9; ++y2) {
					if (&table[x2][y2] != &table[x][y]) {
						if (table[x2][y2] == boxvalue) {
							++failures;
							/* DEBUG printf("y value fail at table[%d][%d]\n", x,y);
							*/
						}
					}
				}

			/* Test x values: Scan row */

				/* DEBUG  printf("Testing x values\n");
				*/

				y2 = y;

				for (x2 = 0; x2 < 9; ++x2) {
					if (&table[x2][y2] != &table[x][y]) {
						if (table[x2][y2] == boxvalue) {
							++failures;
							/* DEBUG  
							printf("x value fail at table[%d][%d]\n", x,y);
							*/
						}
					}
				}


			/* Test block: Start from top left corner of block */
				
				/* DEBUG  printf("Testing block: ");
				*/
			
				x2 = x - x % 3;
				y2 = y - y % 3;

				/* DEBUG  printf("Top left of block: (%d, %d)\n", x2, y2);
				*/
			
				for (ycount = 0; ycount < 3; ++ycount) {
					for (xcount = 0; xcount < 3; ++xcount) {

						/* DEBUG  printf("Block test (x2,y2) (%d, %d); ", x2, y2);
						*/

						if (&table[x2][y2] != &table[x][y]) {
							if (table[x2][y2] == boxvalue) {
								++failures;
							}
						} 
						++x2;
					}
					x2 = x2 - 3; /* Back to left column */
					++y2;
				}
				
				/* DEBUG  
				printf("End of test_value loop: failures = %d\n", failures);
				*/

				if (failures > 0)
					success = 1;
				else 
					success = 0;

			}	/* End of test_value loop */

			/* If success, and exited loop: Set square value to the number */

			/* DEBUG  printf("SUCCESS, table[%d][%d] = %d\n", x, y, boxvalue);
			*/
			table[x][y] = boxvalue;	

		} 		/* End of x loop */ 
	} 			/* End of y loop */

	/* Print the table */

	printf("\n\n");
	for (y = 0; y < 9; ++y) {
		for (x = 0; x < 9; ++x) {
			printf("%d ", table[x][y]);	
		}
		printf("\n");	
	}
	printf("\n\n");

	return (0);
}
