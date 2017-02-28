/* This is maze.c, by Andrew A. Cashner, 2017/02/27.
 *
 * Find a path through a maze
 * Using arrays and stacks
 * Based on Ellis Horowitz et al, *Fundamentals of Data Structures in C* (New
 * York: Computer Science Press, 1993), ch. 3
 *
 * COMPILATION AND USAGE
 * Compile: gcc -Wall -Wextra -ansi -pedantic -o maze.cx maze.c
 * Usage:     ./maze.cx <input file name>
 *
 * PURPOSE
 * This program takes in a maze represented by a text file with a block of 1s
 * and 0s, where the zeros are the open spaces and the ones are the walls.
 * It prints out a visual representation of the maze (for now, on the command
 * line).
 * It then finds a solution for the maze, if there is one, and prints out
 * another representation of the maze showing the successful path through it.
 * If there is no solution, it prints a message saying so.
 *
 * IMPLEMENTATION
 * We represent the maze with a two-dimensional integer array which we typedef
 * as 'maze_array'.  One maze_array, maze, holds the maze itself; the other,
 * mark, tracks all positions we have visited.  The values in these are set with
 * enum flags: MAZE_UNOCCUPIED vs MAZE_OCCUPIED vs (if a square is part of a
 * valid path through the maze) MAZE_PATH, and MARK_UNVISITED vs MARK_VISITED.
 * 
 * We read the maze data from an input file, which for the time being we assume
 * is in the correct format.  The maze is stored as an array of 0s and 1s, with
 * the open spaces as 0s.  The outside borders of the maze are all 1.  The
 * entrance of the maze is at (x,y) position (1,1); the exit is at (MAZE_HEIGHT
 * - 1, MAZE_WIDTH - 1).  We build the maze based on an input file that
 * represents the maze as a block of 1s and 0s:
 *     "111111111\n"
 *     "101011111\n"
 *     ...
 *
 * We use a stack to keep track of the last succesful position we visited; by
 * popping from the stack we can return to the previous succesful position.  We
 * implement a stack here as a simple array of 'element' structs with the row,
 * column, and direction of motion.  We use 'top' to track the top position in
 * the stack.
 *
 * The function find_path moves through the maze and finds a successful path, if
 * there is one.  Starting from the initial position, we test the next position
 * in every direction.  The (x,y) movements for each direction are stored in
 * 'move'.  If the test square is unoccupied and we have not visited (i.e., it
 * is not set to MARK_VISITED in 'mark), then we move there and test for the
 * next position.  Otherwise, we try another direction until we either find an
 * open square, find the exit, or give up.  If we run into a dead end---that is,
 * we have been moving through a series of open spaces but find there is no
 * where else to turn---then we pop positions from our stack ('position_stack')
 * to backtrack and try other moves from each previous position until we find
 * another path.
 *
 * We store the successful path data in 'maze' by setting the proper positions to
 * MAZE_PATH.
 * We can print the maze, with or without path markers, with 'print_maze'.
 * 
 * TODO
 *   - Set up user-input maze size
 *   - Write a program to create the maze to begin with
 *   - Create better graphical interface for creating and displaying the maze.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

/* DEBUGGING *******************************************/
#ifdef DEBUG
#define DEBUG_PRINT(x) printf x
#else
#define DEBUG_PRINT(x) {}; 
#endif

/* GLOBAL DEFINITIONS AND VARIABLES ********************/

#define MAX_STR 80*4 /* String size */
int maze_height, maze_width;
#define MAX_MAZE_WIDTH  100
#define MAX_MAZE_HEIGHT 100
#define MAZE_EXIT_ROW maze_height - 2
#define MAZE_EXIT_COL maze_width - 2
#define MAX_STACK_SIZE MAX_MAZE_WIDTH * MAX_MAZE_HEIGHT

/* We create a maze_array for the maze itself and for 'mark', which tracks
 * positions we've visited.
 */
typedef int maze_array[MAX_MAZE_HEIGHT][MAX_MAZE_WIDTH];

/* Flags for setting the state of the maze and mark arrays. */
enum { 
    MAZE_UNOCCUPIED = false,
    MAZE_OCCUPIED = true,
    MAZE_PATH
} maze_state;
enum { 
    MARK_UNVISITED = false,
    MARK_VISITED = true
} mark_state;

/* Structures and pointers needed for our stack. */
typedef struct element *element_ptr;
typedef struct element {
    short int row, col, dir;
} element;

/* These integer pairs store the moves for each direction.
 * Index[0] is the vertical and [1], the horizontal.
 */
#define MAX_DIRECTIONS 4
int move[MAX_DIRECTIONS][2] = {
    {-1, 0},    /* N */
    {0, 1},     /* E */
    {1, 0},     /* S */
    {0, -1},    /* W */
};
char dir_str[] = "NESW"; /* Used for printing the directions when debugging */

/* ERROR MESSAGES */
enum {
    ERR_ARGS,
    ERR_FILE,
    ERR_INPUT,
    ERR_TOO_HIGH,
    ERR_TOO_WIDE,
    STACK_FULL,
    STACK_EMPTY,
    NO_PATH,
    ERR_UNKNOWN
} error_code;
const char *error_msg[] = {
    "Usage: maze <maze input file name>",
    "Could not open file for reading",
    "Invalid input maze",
    "Input maze too high",
    "Input maze too wide",
    "Stack capacity exceeded.",
    "Stack empty.",
    "The maze does not have a path.",
    "Unknown error"
};

/* FUNCTION PROTOTYPES *********************************/

void exit_error(int error_code);
FILE *setup_input(int argc, char *argv[]);
int *array_value(maze_array maze, int col, int row);
void setup_maze(FILE *infile, maze_array maze, maze_array mark);
void push(element new_element, element stack[], int *top);
element pop(element stack[], int *top);
void print_maze(maze_array maze);
void find_path(maze_array maze, maze_array mark, element_ptr position_stack, int *top);

/* MAIN *************************************************/

int main(int argc, char *argv[]) {
    FILE *infile;
    maze_array maze, mark;
    element position_stack[MAX_STACK_SIZE];
    element_ptr position_stack_ptr = position_stack;
    int top = -1;

    infile = setup_input(argc, argv);
    setup_maze(infile, maze, mark);
    print_maze(maze);
    find_path(maze, mark, position_stack_ptr, &top);
    print_maze(maze);

    return(0);
}

/* FUNCTIONS ************************************/

void exit_error(int error_code) {
    /* Exit the program after printing a message from the array error_msg */
    fprintf(stderr, "%s\n", error_msg[error_code]);
    exit(EXIT_FAILURE);
}

FILE *setup_input(int argc, char *argv[]) {
    /* Validate command-line arguments, open and validate input file */
    FILE *infile;
    char *infile_name;
    if (argc != 2) {
        exit_error(ERR_ARGS);
    }
    infile_name = argv[1];
    infile = fopen(infile_name, "r");
    if (infile == NULL) {
        exit_error(ERR_FILE);
    }
    return(infile);
}

int *array_value(maze_array maze, int col, int row) {
    /* Access a maze array element by coordinates,
     * if the coordinates are valid. */
    assert(col >= 0 && col < maze_height);
    assert(row >= 0 && row < maze_width);
    return(&maze[col][row]);
}

void setup_maze(FILE *infile, maze_array maze, maze_array mark) {
    /* Read and validate maze data, set up mark array to beginning state */

    /* TODO for now we assume the input maze is a correct size
     *      ideally we would build our maze based on the input alone
     */
    int row, col, prev_row_length;
    char line[MAX_STR];
    char input_maze[MAX_MAZE_HEIGHT][MAX_MAZE_HEIGHT];

    col = prev_row_length = 0;
    for (row = 0; fgets(line, sizeof(line), infile) != NULL; ++row) {
        if (col != prev_row_length) {
            /* Make sure all the lines are the same length,
             * ignoring blank lines */
            exit_error(ERR_INPUT);
        }
        for (col = 0; line[col] != '\0'; ++col) {
           /* Make sure input is within correct size and contains only 1s, 0s */
           if (row > MAX_MAZE_HEIGHT) {
               exit_error(ERR_TOO_HIGH);
           } else if (col > MAX_MAZE_WIDTH) {
               exit_error(ERR_TOO_WIDE);
           } else if (line[col] == '\n') {
               if (col > 0) {
                   /* End string and store line length */
                   input_maze[row][col] = '\0';
                   prev_row_length = col;
                   DEBUG_PRINT(("this line length: %d\n", col));
               } else {
                   exit_error(ERR_INPUT);
               }
               break;
           } else if (line[col] == '0' || line[col] == '1') {
               DEBUG_PRINT(("Read: %c |", line[col]));
               input_maze[row][col] = line[col];
           }
        }
    /* TODO deal with trailing newline */
    }
    /* Store dimensions of input maze */
    maze_height = row;
    maze_width = --prev_row_length;
    DEBUG_PRINT(("found maze_height %d maze_width %d\n", maze_height, maze_width));
    assert(maze_height > 0 && maze_height < MAX_MAZE_HEIGHT);
    assert(maze_width > 0 && maze_width < MAX_MAZE_WIDTH);

    for (row = 0; row < maze_height; ++row) {
        for (col = 0; col < maze_width; ++col) {
            if (input_maze[row][col] == '0') {
                *array_value(maze, row, col) = MAZE_UNOCCUPIED;
            } else if (input_maze[row][col] == '1') {
                *array_value(maze, row, col) = MAZE_OCCUPIED;
            } 
        }
    }

    /* Populate the 'mark' array with 0s, except for the borders */
    for (row = 0; row < maze_height; ++row) {
        for (col = 0; col < maze_width; ++col) {

            if (row > 0 && row < maze_height - 1 &&
                    col > 0 && col < maze_width - 1) {
                *array_value(mark, row, col) = MARK_UNVISITED;
            } else {
                *array_value(mark, row, col) = MARK_VISITED;
            }
        }
    }
    return;
}

void push(element new_element, element_ptr stack, int *top) {
    /* Add an element to the top of the stack */
    if (*top > MAX_STACK_SIZE) {
        exit_error(STACK_FULL);
    } 
    ++(*top);
    stack[*top] = new_element;
    return;
}

element pop(element_ptr stack, int *top) {
    /* Fetch the top element of the stack and set 'top' so that the next 'push'
     * command will replace this element */
    element next_element;
    if (*top == -1) {
        exit_error(STACK_EMPTY);
    }
    next_element = stack[*top];
    --(*top);
    return(next_element);
}

void print_maze(maze_array maze) {
    /* Print the maze to stdout showing walls, openings, and the path through if
     * one has been found */
    int i, j;
    char *block_str[] = {   /* Characters to print, matches maze_state enum */
        "   ",              /* MAZE_UNOCCUPIED  */
        "[@]",              /* MAZE_OCCUPIED    */ 
        " * "               /* MAZE_PATH        */
    };
    int block_str_index;

    assert(maze_height > 0 && maze_height < MAX_MAZE_HEIGHT);
    assert(maze_width > 0 && maze_width < MAX_MAZE_WIDTH);
    
    printf("\n");
    for (i = 0; i < maze_width; ++i) {
        for (j = 0; j < maze_height; ++j) {
            block_str_index = *array_value(maze, i, j);
            printf("%s", block_str[block_str_index]);
        }
        printf("\n");
    }
    printf("\n");
    return;
}

void find_path(maze_array maze, maze_array mark, element_ptr position_stack, int *top) {
    /* Output a path through the maze if there is one (adapted from Horowitz) */
    int i, row, col, next_row, next_col, dir;
    bool found = false;
    element position;
    *top = 0;
    
    /* Set start position and direction; initialize stack with this position.
     * Since we start in top left corner, there is no way we can move north, so
     * start at east (move[1]) instead
     */
    *array_value(mark, 1, 1) = MARK_VISITED;
    position_stack[0].row = position_stack[0].col = 1;
    position_stack[0].dir = 1; 

    while (*top > -1 && found == false) {
        /* Get last good fallback position */
        position = pop(position_stack, top);
        row = position.row;
        col = position.col;
        dir = position.dir;
        DEBUG_PRINT(("Fallback: row %d col %d dir %c\n", row, col, dir_str[dir]));

        while (dir < MAX_DIRECTIONS && found == false) {
            /* Test for valid moves in next direction, given in 'dir' */
            next_row = row + move[dir][0];
            next_col = col + move[dir][1];
            DEBUG_PRINT(("Try: row %d col %d dir %c\n", row, col, dir_str[dir]));

            if (next_row == MAZE_EXIT_ROW && next_col == MAZE_EXIT_COL) {
                /* We found the exit and we're done */
                found = true;
            } else if (*array_value(maze, next_row, next_col) == MAZE_UNOCCUPIED &&
                    *array_value(mark, next_row, next_col) == MARK_UNVISITED) {
                /* We found an unoccupied, unvisited square: Move to it */
                *array_value(mark, next_row, next_col) = MARK_VISITED;
                position.row = row;
                position.col = col;
                ++dir; /* Oscillate through directions in saved position */
                position.dir = dir;
                /* Save this in the stack as a fallback position */
                push(position, position_stack, top);
                
                row = next_row;
                col = next_col;
                dir = 0; /* Reset directions for test loop */

            } else {
                ++dir; /* Try another direction */
            }
        }
    }
    if (found == true) {
        /* Save the path in the maze for printing */
        for (i = 0; i <= *top; ++i) {
            *array_value(maze, position_stack[i].row, position_stack[i].col) = MAZE_PATH;
        }
        /* Also current position and the exit position */
        *array_value(maze, row, col) = MAZE_PATH;
        *array_value(maze, MAZE_EXIT_ROW, MAZE_EXIT_COL) = MAZE_PATH;
    } else {
        exit_error(NO_PATH);
    }
    return;
}


