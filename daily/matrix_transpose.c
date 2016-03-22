/* matrix_transpose.c -- based on Horowitz p. 68,72 
 * Cashner 2014-11-15 
 */

/* Read in a matrix. 
 * Store values as triples in sparse matrix data structure, matrix a.
 * Transpose matrix a to matrix b (sparse representation).
 * Write out new matrix b in full representation.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_TERMS 101	/* Maximum number of terms + 1 */
#define MAX_COL 50 	/* Maximum number of columns + 1 */
#define MAX_CHARS 250

typedef struct {
	int col;
	int row;
	int value;
	} term;

void fast_transpose(term[], term[]);

void print_matrix(term[]);

int main(void) 
{
	char line[MAX_CHARS];
	char number_string[10];
	int i, rows, cols, values, chars, number_int;
	term matrix_a[MAX_TERMS], matrix_b[MAX_TERMS];
	
	rows = cols = values = chars = number_int = 0;
	/* Read in matrix_a and store as triples */
	printf("Enter matrix by rows.\n");
	printf("Separate each value by comma, each row by semicolon.\n");
	/* Example: 1, 0, 31; 0, 15, 0; 27, 0, 9; */
	fgets(line, sizeof(line), stdin);
	for (i = 0; line[i] != '\0'; ++i) {
		if (line[i] == ' ') {
			continue;
		} else if ((line[i] == ',') ||
			 (line[i] == ';') ||
			 (line[i] == '\n')) {
		
			number_string[chars] = '\0';
			chars = 0;
			sscanf(number_string, "%d", &number_int);
			if (number_int != 0) {
				++values;
				matrix_a[values].value = number_int;
				matrix_a[values].col = cols;
				matrix_a[values].row = rows;
			}
			++cols;
			if ((line[i] == ';') || (line[i] == '\n')) {
				matrix_a[0].col = cols;
				cols = 0;
				++rows; 
			} 
		} else if ((line[i] >= '0') && (line[i] <= '9')) {
			number_string[chars++] = line[i];
		} else {
			fprintf(stderr, "Invalid character %c.\n", line[i]);
			exit(EXIT_FAILURE);
		}
	}
	matrix_a[0].row = rows;
	matrix_a[0].value = values;

	print_matrix(matrix_a);

	fast_transpose(matrix_a, matrix_b);

	print_matrix(matrix_b);

	return(0);
}

void fast_transpose(term a[], term b[]) 
{
/* The transpose of a is placed in b */
	int row_terms[MAX_COL], start_pos[MAX_COL];
	int i, j, num_cols = a[0].col, num_terms = a[0].value;
	
	b[0].row = num_cols;
	b[0].col = a[0].row;
	b[0].value = num_terms;

	if (num_terms > 0) { /* Nonzero matrix */
		for (i = 0; i < num_cols; i++)
			row_terms[i] = 0;
		for (i = 1; i <= num_terms; i++)
			row_terms[a[i].col]++;
		start_pos[0] = 1;
		for (i = 1; i < num_cols; i++)
			start_pos[i] = start_pos[i - 1] + row_terms[i - 1];
		for (i = 1; i <= num_terms; i++) {
			j = start_pos[a[i].col]++;
			b[j].row = a[i].col;
			b[j].col = a[i].row;
			b[j].value = a[i].value;
		}
	}
}

void print_matrix(term a[])
{
/* Print matrix stored as triples as a proper matrix to STDOUT */

	int row_terms[MAX_COL], start_pos[MAX_COL];
	int i, j, num_cols = a[0].col, num_terms = a[0].value;

	b[0].row = num_cols;
	b[0].col = a[0].row;
	b[0].value = num_terms;

	if (num_terms > 0) { /* Nonzero matrix */
		for (i = 0; i < num_cols; i++)
			row_terms[i] = 0;
		for (i = 1; i <= num_terms; i++)
			row_terms[a[i].col]++;
		start_pos[0] = 1;
		for (i = 1; i < num_cols; i++)
			start_pos[i] = start_pos[i - 1] + row_terms[i - 1];
/*		for (i = 1; i <= num_terms; i++) {
			j = start_pos[a[i].col]++;
			if (lpr_row == i && lpr_col = j)
				
				lpr_out[lpr_row * num_cols + lpr_col] = 
			
*/
		}
	}
/*	int i, j, k, n, current_value, current_col;
	int num_rows, num_cols, num_terms;
	int row_terms[MAX_COL], start_pos[MAX_COL];
	char lpr_out[MAX_CHARS];
	i = j = k = n = current_value = current_col = 0;
	num_rows = a[0].row;
	num_cols = a[0].col;
	num_terms = a[0].value;

	if (num_terms == 0) {
		fprintf(stderr, "Error: Empty matrix\n");
		return;
	}
*/
/* Debug*/
	for (i = 0; i <= num_terms; ++i)
		printf("\n[i %d] row %d col %d val %d\n", i, a[i].row, a[i].col,
		a[i].value);
/* end debug */	
/*	for (i = 0; i < num_rows; ++i) {
		for (j = 0; j < num_cols; ++j) {
			printf("X ");
		}
		printf("\n");
	}


	for (i = 0; i < num_cols; ++i)
		row_terms[i] = 0;
	for (i = 1; i <= num_terms; ++i) {
		row_terms[a[i].col]++;
	}
	start_pos[0] = 1;
	for (i = 1; i < num_cols; ++i) {
		start_pos[i] = start_pos[i - 1] + row_terms[i - 1];
	}
	start_pos[i - 1] = '\0';

	for (i = 0; start_pos[i] != '\0'; ++i)
		printf("start_pos[i %d] %d\n", i, start_pos[i]);
	for (i = 0; i < num_rows; ++i) {
		printf("row %d ", i);
		for (n = start_pos[i]; n < start_pos[i + 1]; ++n) {
			printf("start_pos %d ", n);
			for (j = 0; j < num_cols; ++j) {
				printf("col %d ", j);

				current_col = a[start_pos[n]].col;
				if (j == current_col) {
					current_value = a[start_pos[i]].value;
					sprintf(&lpr_out[k], "%d", current_value);
				} else {
					lpr_out[k] = '0';
				}
				++k;
				lpr_out[k] = ' ';
			}
		}
		lpr_out[k++] = '\n';
	}
	lpr_out[k] = '\0';
	printf("%s\n", lpr_out);
*/
	return;
}


	

