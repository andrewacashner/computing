#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_CHAR 80
#define MAX_LINES 40
#define MATRIX_NULL -1

void fill_matrix(char input[MAX_LINES][MAX_CHAR], 
        int matrix[MAX_LINES][MAX_LINES]);
void print_matrix(int matrix[MAX_LINES][MAX_LINES]);
void rotate_matrix(int orig_matrix[MAX_LINES][MAX_LINES], 
        int rotated_matrix[MAX_LINES][MAX_LINES]);

int main(int argc, char **argv) {
    char *infile_name = "";
    FILE *infile = NULL;
    char line[MAX_CHAR] = "";
    char input_buffer[MAX_LINES][MAX_CHAR];
    int matrix[MAX_LINES][MAX_LINES];
    int rotated_matrix[MAX_LINES][MAX_LINES];
    int i, j;

    if (argc != 2) {
        fprintf(stderr, "Usage: rotate [FILENAME]\n");
        exit(EXIT_FAILURE);
    }

    infile_name = argv[1];
    infile = fopen(infile_name, "r");
    if (infile == NULL) {
        fprintf(stderr, "Could not open file %s for reading.\n", infile_name);
        exit(EXIT_FAILURE);
    }

    for (i = 0; i < MAX_LINES; ++i) {
        for (j = 0; j < MAX_LINES; ++j) {
            matrix[i][j] = MATRIX_NULL;
            rotated_matrix[i][j] = MATRIX_NULL;
        }
    }

    for (i = 0; fgets(line, sizeof(line), infile) != NULL; ++i) {
        strcpy(input_buffer[i], line);
    }

    fill_matrix(input_buffer, matrix);

    print_matrix(matrix);

    for (i = 0; i < 2; ++i) {
        rotate_matrix(matrix, rotated_matrix);
        print_matrix(rotated_matrix);

        rotate_matrix(rotated_matrix, matrix);
        print_matrix(matrix);
    }

    fclose(infile);
    return(0);
}

void fill_matrix(char input[MAX_LINES][MAX_CHAR], 
        int matrix[MAX_LINES][MAX_LINES]) {

    int i, j, matrix_j;

    for (i = 0; i < MAX_LINES; ++i) {
        for (j = 0, matrix_j = 0; j < MAX_CHAR; ++j) {
            switch (input[i][j]) {
                case '1':
                    matrix[i][matrix_j] = 1;
                    ++matrix_j;
                    break;

                case '0':
                    matrix[i][matrix_j] = 0;
                    ++matrix_j;
                    break;
                
                case '\n':
                    matrix[i][matrix_j] = MATRIX_NULL;
                    ++matrix_j;
                    break;

                default:
                    continue;
            }
        }
    }
}

char matrix_value_display(int value) {
    return value == 0 ? '.' : '*';
}

void print_matrix(int matrix[MAX_LINES][MAX_LINES]) {
    int i, j;
    const char *row_separator = "\n";
    const char *col_separator = "";

    for (i = 0; i < MAX_LINES && matrix[i][0] != MATRIX_NULL; ++i) {
        for (j = 0; j < MAX_LINES && matrix[i][j] != MATRIX_NULL; ++j) {
            printf("%c%s", matrix_value_display(matrix[i][j]), col_separator);
        }
        printf("%s", row_separator);
    }
}

void rotate_matrix(int orig_matrix[MAX_LINES][MAX_LINES], 
        int rotated_matrix[MAX_LINES][MAX_LINES]) {

    int i, j, size;
    i = 0;
    for (i = 0; i < MAX_LINES && orig_matrix[0][i] != MATRIX_NULL; ++i) {
        /* Just count */
    }
    size = i;

    for (i = 0; i < size; ++i) {
        for (j = 0; j < size; ++j) {
            rotated_matrix[j][size - i - 1] = orig_matrix[i][j];
        }
    }
}
