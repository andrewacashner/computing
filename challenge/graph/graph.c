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

#define BITMAP_COORD(x, y) bitmap[y][x]

char *read_input(char*);
int y_is_x(int);
int y_is_x_squared(int);
int y_is_x_cubed(int);

int (*select_function(char*))(int);
void print_graph(char*, bool[MAX_ROW][MAX_COL]);

int main(int argc, char *argv[]) {
    char input[MAX_CHAR];
    bool bitmap[MAX_ROW][MAX_COL] = {{false}};
    int (*fn)(int);

    read_input(input);
    fn = select_function(input);

    for (int x = X_MIN; x < X_MAX; x += INCREMENT) {
        int y = (*fn)(x);
        printf("y(%d) = %d\n", x, y);

        if (abs(y) < Y_MAX) {
            printf("Enter value %d at array[%d][%d]\n",
                    y, X_ORIGIN + x, Y_ORIGIN - y);
            bitmap[Y_ORIGIN - y][X_ORIGIN + x] = true;
        } else {
            printf("Y value %d out of range\n", y);
        }
    }

    print_graph(input, bitmap);

    return 0;
}

int y_is_x(int x) {
    int y = x;
    return y;
}

int y_is_x_squared(int x) {
    int y = pow(x, 2);
    return y;
}

int y_is_x_cubed(int x) {
    int y = pow(x, 3);
    return y;
}

int (*select_function(char *fn_text))(int) {
    int (*fn)(int);

    bool string_matches(char *s1, char *s2) {
        return strcmp(s1, s2) == 0;
    }

    if (string_matches(fn_text, "x")) {
        fn = y_is_x;
    } else if (string_matches(fn_text, "x^2")) {
        fn = y_is_x_squared;
    } else if (string_matches(fn_text, "x^3")) {
        fn = y_is_x_cubed;
    } else {
        fprintf(stderr, "Unknown function y = %s\n", fn_text);
        exit(EXIT_FAILURE);
    }
    
    return fn;
}

char *read_input(char *buffer) {
    printf("y = ");
    fgets(buffer, sizeof(buffer), stdin);
    buffer[strlen(buffer) - 1] = '\0';
    return buffer;
}

void print_graph(char *function_name, bool bitmap[MAX_ROW][MAX_COL]) {

    char *this_char;

    for (int y = 0; y < MAX_ROW; ++y) {
        for (int x = 0; x < MAX_COL; ++x) {
            // Print 3 x chars for every 1 y for semi-square output
            if (BITMAP_COORD(x, y) == true) {
                this_char = " * ";
            } else if (y == X_ORIGIN && x == Y_ORIGIN) {
                this_char = " + ";
            } else if (y == X_ORIGIN) {
                this_char = "---";
            } else if (x == Y_ORIGIN) {
                this_char = " | ";
            } else {
                this_char = "   ";
            }
            printf("%s", this_char);
        }
        printf("\n");
    }
    printf("y = %s\n", function_name);
}


