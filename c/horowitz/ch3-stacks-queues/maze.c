/* maze.c -- Andrew A. Cashner, 2017/02/25
 * Find a path through a maze
 * Using arrays and stacks
 * Horowitz *Data Structures* ch 3
 */

/* TODO
 * get rid of global variables
 * write stack functions: create, push, pop
 * write path-finding function from Horowitz
 * set up user-input maze size
 * TODO++
 * write a program to create the maze to begin with
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

/* GLOBAL DEFINITIONS AND VARIABLES ********************/

#define MAX_STR 80

/* The maze is stored as an array of 0s and 1s,
 * with the open spaces as 0s.
 * The outside borders of the maze are all 1.
 * The entrance of the maze is at (x,y) position (1,1);
 * the exit is at (MAZE_HEIGHT - 1, MAZE_WIDTH - 1).
 * We build the maze based on an input file that 
 * represents the maze as a block of 1s and 0s:
 * "111111111\n"
 * "101011111\n"
 * ...
 */
#define MAZE_WIDTH  10
#define MAZE_HEIGHT 10
#define MAZE_EXIT_ROW MAZE_HEIGHT - 2
#define MAZE_EXIT_COL MAZE_WIDTH - 2

int maze[MAZE_HEIGHT][MAZE_WIDTH];
enum { 
    MAZE_UNOCCUPIED = false,
    MAZE_OCCUPIED = true
} maze_state;

/* The 'mark' arrray stores the positions we have visited. */
int mark[MAZE_HEIGHT][MAZE_WIDTH];
enum { 
    MARK_UNVISITED = false,
    MARK_VISITED = true
} mark_state;


/* We use a stack to keep track of the last succesful position we visited;
 * by popping from the stack we can return to the previous succesful position.
 * We implement a stack here as a simple array of 'element' structs with the 
 * row, column, and direction of motion.
 * We use 'top' to track the top position in the stack.
 */
#define MAX_STACK_SIZE MAZE_WIDTH*MAZE_HEIGHT
#define STACK_EMPTY -99 /* Error code */
typedef struct {
    short int row, col, dir;
} element;
element stack[MAX_STACK_SIZE];
int top = -1;

/* We will create 'move', an array of integer pairs, to store the 
 * moves for each direction.
 */
#define MAX_DIRECTIONS 8
#define MOVE_VERT(x)  move[x][0]
#define MOVE_HORIZ(x) move[x][1]
int move[MAX_DIRECTIONS][2] = {
    {-1, 0},    /* N  */
    {-1, 1},    /* NE */
    {0, 1},     /* E  */
    {1, 1},     /* SE */
    {1, 0},     /* S  */
    {1, -1},    /* SW */
    {0, -1},    /* W  */
    {-1, -1}    /* NW */
};

/* Error messages */
enum {
    ERR_ARGS,
    ERR_FILE,
    ERR_INPUT,
    ERR_TOO_HIGH,
    ERR_TOO_WIDE,
    STACK_FULL,
    ERR_UNKNOWN
} error_code;
const char *error_msg[] = {
    "Usage: maze <maze input file name>",
    "Could not open file for reading",
    "Invalid input maze",
    "Input maze too high",
    "Input maze too wide",
    "Stack capacity exceeded.",
    "Unknown error"
};

/* FUNCTION PROTOTYPES *********************************/

void exit_error(int error_code);
void push(element new_element, element stack[], int *top);
void print_maze(void);
void find_path(void);



/* MAIN *************************************************/

int main(int argc, char *argv[]) {
    FILE *infile;
    char *infile_name;
    char line[MAX_STR];
    int i, j;
   
    /* Open input file to get maze data */
    if (argc != 2) {
        exit_error(ERR_ARGS);
    }
    infile_name = argv[1];
    infile = fopen(infile_name, "r");
    if (infile == NULL) {
        exit_error(ERR_FILE);
    }
    /* Read and validate maze date */
    /* TODO for now we assume the input maze is a correct size
     *      ideally we would build our maze based on the input alone
     */
    for (i = 0; fgets(line, sizeof(line), infile) != NULL; ++i) {
        for (j = 0; line[j] != '\n'; ++j) {
            if (i > MAZE_HEIGHT) {
                exit_error(ERR_TOO_HIGH);
            } else if (j > MAZE_WIDTH) {
                exit_error(ERR_TOO_WIDE);
            }
            if (line[j] == '0') {
                maze[i][j] = MAZE_UNOCCUPIED;
            } else if (line[j] == '1') {
                maze[i][j] = MAZE_OCCUPIED;
            } else {
                exit_error(ERR_INPUT);
            }
        }
    }

    /* Populate the 'mark' array with 0s, except for the borders */
    for (i = 0; i < MAZE_HEIGHT; ++i) {
        for (j = 0; j < MAZE_WIDTH; ++j) {

            if (i > 0 && i < MAZE_HEIGHT - 1 &&
                    j > 0 && j < MAZE_HEIGHT - 1) {
                mark[i][j] = MARK_UNVISITED;
            } else {
                mark[i][j] = MARK_VISITED;
            }
        }
    }

    print_maze();
   
    find_path();

    return(0);
}

/* FUNCTIONS ************************************/

void exit_error(int error_code) {
    fprintf(stderr, "%s\n", error_msg[error_code]);
    exit(EXIT_FAILURE);
}

void push(element new_element, element stack[], int *top) {
    if (*top > MAX_STACK_SIZE) {
        exit_error(STACK_FULL);
    } 
    stack[*top] = new_element;
    ++(*top);
    return;
}

element pop(element stack[], int *top) {
    element new_element;
    if (*top == -1) {
        new_element.row = STACK_EMPTY;
    }
    new_element = stack[*top];
    --(*top);
    return(new_element);
}

void print_maze(void) {
    int i, j;
    for (i = 0; i < MAZE_HEIGHT; ++i) {
        for (j = 0; j < MAZE_HEIGHT; ++j) {
            printf("%s", maze[i][j] == MAZE_OCCUPIED ? "[]" : "  "); 
        }
        printf("\n");
    }
    return;
}

void find_path(void) {
    /* Output a path through the maze if there is one */
    int i, row, col, next_row, next_col, dir;
    bool found = false;
    element position;

    mark[1][1] = 1;
    top = 0;
    stack[0].row = stack[0].col = 1;
    stack[0].dir = 0;

    while (top > -1 && found == false) {
        position = pop(stack, &top);

        row = position.row;
        col = position.col;
        dir = position.dir;

        while (dir < MAX_DIRECTIONS && found == false) {
            /* Move in next direction, given in 'dir' */
            next_row = row + MOVE_VERT(dir);
            next_col = col + MOVE_HORIZ(dir);
            if (next_row == MAZE_EXIT_ROW && next_col == MAZE_EXIT_COL) {
                found = true;
            } else if (maze[next_row][next_col] == MAZE_UNOCCUPIED &&
                    mark[next_row][next_col] == MARK_UNVISITED) {

                mark[next_row][next_col] = MARK_VISITED;
                position.row = row;
                position.col = col;
                ++dir;
                position.dir = dir;
               
                push(position, stack, &top);
                row = next_row;
                col = next_col;
                dir = 0;

            } else {
                ++dir;
            }
        }
    }
    if (found == true) {
        printf("The path is:\n");
        printf("row  col\n");
        for (i = 0; i <= top; ++i) {
            printf("%2d%5d\n", stack[i].row, stack[i].col);
        }
        printf("%2d%5d\n", row, col);
        printf("%2d%5d\n", MAZE_EXIT_ROW, MAZE_EXIT_COL);
    } else {
        printf("The maze does not have a path.\n");
    }
    return;
}


