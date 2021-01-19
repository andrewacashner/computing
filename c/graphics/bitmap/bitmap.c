/* Print out a simple bitmap in the terminal. 
 * Start with graphic the equation x = y.
 * */
#include <stdio.h>
#include <limits.h>

#define FACTOR 4
#define ROWS CHAR_BIT * FACTOR
#define COLS_BITS ROWS
#define COLS_BYTES FACTOR

#define X_COORD(row) ROWS - row - 1
#define Y_COORD(col, bit) col * CHAR_BIT + bit

#define BMP_DO(row, col, bit, fn) {\
    for (row = 0; row < ROWS; ++row) {\
        for (col = 0; col < COLS_BYTES; ++col) {\
            for (bit = 0; bit < CHAR_BIT; ++bit) {\
                fn;\
            }\
        }\
    }\
}\

#define SET_BIT(var, bit) {var |= 1 << bit;}

#define GRAPH(fn, array, row, col, bit) {\
    BMP_DO(row, col, bit, {\
        if (fn) {\
            SET_BIT(array[row][col], bit);\
        }\
    });\
}

typedef unsigned char bmp[ROWS][COLS_BYTES];

void bmp_initialize(bmp, int, int);
void bmp_print(bmp, int, int);

int main(void) {
    bmp bitmap;
    int row, col, bit;

    bmp_initialize(bitmap, ROWS, COLS_BYTES); 

    /* Outline */
    GRAPH((X_COORD(row) == 0), 
            bitmap, row, col, bit);
    GRAPH((X_COORD(row) == ROWS - 1), 
            bitmap, row, col, bit);

    GRAPH((Y_COORD(col, bit) == 0), 
            bitmap, row, col, bit);
    GRAPH((Y_COORD(col, bit) == COLS_BITS - 1), 
            bitmap, row, col, bit);

    /* Greek Cross */
    GRAPH((X_COORD(row) == (ROWS - 1) / 2), 
            bitmap, row, col, bit);
    GRAPH((Y_COORD(col, bit) == (COLS_BITS - 1) / 2), 
            bitmap, row, col, bit);

    /* St. Andrew's Cross */
    GRAPH((X_COORD(row) == Y_COORD(col, bit)), 
            bitmap, row, col, bit);
    GRAPH((row == Y_COORD(col, bit)), 
            bitmap, row, col, bit);
    
    bmp_print(bitmap, ROWS, COLS_BYTES);
    
    return(0);
}

/* Set all the values of a 2-dimensional character array to zero. */
void bmp_initialize(bmp bitmap, int rows, int cols_bytes) {
    int row, col;
    
    for (row = 0; row < rows; ++row) {
        for (col = 0; col < cols_bytes; ++col) {
            bitmap[row][col] = 0;
        }
    }
    return;
}

void bmp_print(bmp bitmap, int rows, int cols_bytes) {
    int row, col, bit;

    for (row = 0; row < rows; ++row) {
        for (col = 0; col < cols_bytes; ++ col) {
            for (bit = 0; bit < CHAR_BIT; ++bit) {
                if ((bitmap[row][col] & (1 << bit))) {
                    printf("X ");
                } else {
                    printf(". ");
                }
            }
        }
        printf("\n");
    }
    return;
}


    
