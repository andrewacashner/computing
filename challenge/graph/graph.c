#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <math.h>

#define MAX_CHAR 80

#define MAX_ROW 40
#define MAX_COL 40

#define X_ORIGIN MAX_ROW / 2
#define X_MIN -X_ORIGIN
#define X_MAX X_ORIGIN

#define Y_ORIGIN MAX_COL / 2 
#define Y_MIN -Y_ORIGIN
#define Y_MAX Y_ORIGIN

#define INCREMENT 1

int yIsX(int x);
int yIsXSquared(int x);

int main(int argc, char *argv[]) {
    char input[MAX_CHAR];
    int row, col;
    bool bitmap[MAX_ROW][MAX_COL] = {{false}};
    int x;
    int (*fn)(int);

    printf("y = ");
    fgets(input, sizeof(input), stdin);
    input[strlen(input) - 1] = '\0';

    if (strcmp(input, "x") == 0) {
        fn = yIsX;
    } else if (strcmp(input, "x^2") == 0) {
        fn = yIsXSquared;
    } else {
        fprintf(stderr, "Unknown function y = %s\n", input);
        exit(EXIT_FAILURE);
    }

    for (x = X_MIN; x <= X_MAX; x += INCREMENT) {
        int y = fn(x);
        printf("y(%d) = %d\n", x, y);

        if (abs(y) <= Y_MAX) {
            printf("Enter value %d at array[%d][%d]\n",
                    y, X_ORIGIN + x, Y_ORIGIN - y);
            bitmap[X_ORIGIN + x][Y_ORIGIN - y] = true;
        } else {
            printf("Y value %d out of range\n", y);
        }
    }

    for (row = 0; row < MAX_ROW; ++row) {
        for (col = 0; col < MAX_COL; ++col) {
            char thisValue = bitmap[row][col] ? '*' : ' ';
            printf("%c", thisValue);
        }
        printf("\n");
    }
    
    printf("y = %s\n", input);

    return 0;
}

int yIsX(int x) {
    int y = x;
    return y;
}

int yIsXSquared(int x) {
    int y = pow(x, 2);
    return y;
}


