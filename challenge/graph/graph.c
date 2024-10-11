/* Rudimentary graphing program
 * 
 * Graphs one of a preset library of functions to the terminal.
 * Reads the functions from user input (but only to match them to one
 * already programmed).
 * Plots the functions for integer values between X_MIN and X_MAX.
 *
 * Andrew Cashner, 2024/10/11
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <math.h>

#define MAX_CHAR 80

#define MAX_ROW 25
#define MAX_COL 25

#define X_ORIGIN MAX_ROW / 2
#define X_MIN -X_ORIGIN
#define X_MAX X_ORIGIN

#define Y_ORIGIN MAX_COL / 2 
#define Y_MIN -Y_ORIGIN
#define Y_MAX Y_ORIGIN

#define INCREMENT 1

#define GRAPH_COORD(x, y) graph[y][x]

char *read_input(char*);
int y_is_x(int);
int y_is_x_squared(int);
int y_is_x_cubed(int);

int (*select_function(char*))(int);
void set_graph(bool[MAX_ROW][MAX_COL], int(*)(int));
void print_graph(char*, bool[MAX_ROW][MAX_COL]);

int main(int argc, char *argv[]) {
    char input[MAX_CHAR];
    bool graph[MAX_ROW][MAX_COL] = {{false}};
    int (*fn)(int);

    read_input(input);
    fn = select_function(input);
    set_graph(graph, fn);
    print_graph(input, graph);

    return 0;
}

char *read_input(char *buffer) {
    printf("y = ");
    fgets(buffer, sizeof(buffer), stdin);
    buffer[strlen(buffer) - 1] = '\0';
    return buffer;
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

void set_graph(bool graph[MAX_ROW][MAX_COL], int (*fn)(int)) {
    for (int x = X_MIN; x < X_MAX; x += INCREMENT) {
        int y = (*fn)(x);

        if (abs(y) < Y_MAX) {
            GRAPH_COORD(X_ORIGIN + x, Y_ORIGIN - y) = true;
        } 
    }
}

void print_graph(char *function_name, bool graph[MAX_ROW][MAX_COL]) {
    enum graph_point_type { SPACE, ORIGIN, X_AXIS, Y_AXIS, POINT } type;

    // Print 3 x chars for every 1 y for semi-square output
    char *graph_point_str[] = {
        "   ", // SPACE
        " + ", // ORIGIN
        "---", // X_AXIS
        " | ", // Y_AXIS
        " * "  // POINT
    };

    for (int y = 0; y < MAX_ROW; ++y) {
        for (int x = 0; x < MAX_COL; ++x) {
            if (GRAPH_COORD(x, y) == true) {
                type = POINT;
            } else if (y == X_ORIGIN && x == Y_ORIGIN) {
                type = ORIGIN;
            } else if (y == X_ORIGIN) {
                type = X_AXIS;
            } else if (x == Y_ORIGIN) {
                type = Y_AXIS;
            } else {
                type = SPACE;
            }
            printf("%s", graph_point_str[type]);
        }
        printf("\n");
    }

    printf("y = %s\n", function_name);
}


