/****************************************************
 * CDOKU : Generates sudoku tables.					*
 *												 	*
 * Andrew Cashner, 2014-01-01						*
 ***************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int rand_lim(int limit);
void print(int table[9][9]);

int table[9][9];				/* 9x9 sudoku grid */

/***************************************************************************/

int main()
{
	int boxvalue;					/* Value for each square */
	int x;							/* X coordinate */
	int y;							/* Y coordinate */
	int x2;							/* X coord. of test cell */
	int y2; 						/* Y coord. of test cell */
	int failures = 0;				/* Count failed tests */
	int success = 0 ;				/* Evaluator for tests */

	/* Seed random-number generator */
	srand(time(NULL));				

	/* Index through every square in matrix and set values */
	for (y = 0; y < 9; ++y) {
		for (x = 0; x < 9; ++x) {
	
			/* Fill the board with random values */
			boxvalue = rand_lim(8) + 1;
			table[x][y] = boxvalue;	
		}
	}
	
	print(table);

	/* Test values in table and replace as needed */
	while (success != 0) { /* Skipping this section: this should be == 0 */
		for (y=0; y < 9; ++y) {
			for (x = 0; x < 9; ++x) {

				/* Test x values (row) */
				x2 = x;
				for (y2 = 0; y2 < 9; ++y2) {
					if (&table[x2][y2] != &table[x][y]) {
						if (table[x2][y2] == table[x][y]) {
							++failures;
							boxvalue = rand_lim(8) + 1;
							table[x][y] = boxvalue;
						}
					}
					if (failures > 0)
						success = 1;
				}
			}
		}
	} 


			/* Test y values (column) */

			/* Test block */

	return (0);
}


/* FUNCTION rand_lim -- return a random number between 0 and limit inclusive */
int rand_lim(int limit) {

	int divisor = RAND_MAX/(limit+1);
	int return_value;

	do {
		return_value = rand() / divisor;
	} 	while (return_value > limit); 

	return (return_value);
}

/* FUNCTION print -- Print the table */
void print(int table[9][9]) {
	int x, y;	/* X & Y dimension */

	printf("\n\n");
	for (y = 0; y < 9; ++y) {
		for (x = 0; x < 9; ++x) {
			if ((x == 3) || (x == 6)) {
				printf("| ");
			}
			printf("%d ", table[x][y]);	
			}
		printf("\n");
		if ((y == 2) || (y == 5)) {
			printf("\n");
		}
	}
	printf("\n\n");
	return;
}
