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

/* FUNCTION PROTOTYPES *********************************/
void exit_error(int error_code, char *extra_info);
void print_maze(void);
void find_path(void);

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
#define MAZE_EXIT maze[MAZE_HEIGHT - 2][MAZE_WIDTH - 2]
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
typedef struct {
    short int row, col, dir;
} element;
element stack[MAX_STACK_SIZE];
int top; 

/* We will create 'move', an array of integer pairs, to store the 
 * moves for each direction.
 */
#define MAX_DIRECTIONS 8
enum {
    VERT = 0,
    HORIZ
} move_index;
int move[MAX_DIRECTIONS][2] = {
    {-1, 0},    /* N  */
    {-1, -1},   /* NE */
    {0, 1},     /* E  */
    {1, 1},     /* SE */
    {1, 0},     /* SW */
    {1, -1},    /* W  */
    {0, -1},    /* NW */
    {-1, -1}
};

/* Error messages */
enum {
    ERR_ARGS,
    ERR_FILE,
    ERR_INPUT,
    ERR_TOO_HIGH,
    ERR_TOO_WIDE,
    ERR_UNKNOWN
} error_code;
const char *error_msg[] = {
    "Usage: maze <maze input file name>",
    "Could not open file for reading: ",
    "Invalid input maze",
    "Input maze too high",
    "Input maze too wide",
    "Unknown error"
};



/* MAIN *************************************************/

int main(int argc, char *argv[]) {
    FILE *infile;
    char *infile_name;
    char line[MAX_STR];
    int i, j;
    bool is_occupied;
   
    /* Open input file to get maze data */
    if (argc != 2) {
        exit_error(ERR_ARGS, NULL);
    }
    infile_name = argv[1];
    infile = fopen(infile_name, "r");
    if (infile == NULL) {
        exit_error(ERR_FILE, NULL);
    }
    /* Read and validate maze date */
    /* TODO for now we assume the input maze is a correct size
     *      ideally we would build our maze based on the input alone
     */
    for (i = 0; fgets(line, sizeof(line), infile) != NULL; ++i) {
        for (j = 0; line[j] != '\n'; ++j) {
            if (i > MAZE_HEIGHT) {
                exit_error(ERR_TOO_HIGH, NULL);
            } else if (j > MAZE_WIDTH) {
                exit_error(ERR_TOO_WIDE, NULL);
            }
            if (line[j] == '1') {
                is_occupied = true;
            } else if (line[j] == '0') {
                is_occupied = false;
            } else {
                exit_error(ERR_INPUT, NULL);
            }
            maze[i][j] = is_occupied;
        }
    }

    /* Populate the 'mark' array with 0s, except for the borders */
    for (i = 0; i < MAZE_HEIGHT; ++i) {
        for (j = 0; j < MAZE_WIDTH; ++j) {

            if (i > 0 && i < MAZE_HEIGHT - 1 &&
                    j > 0 && j < MAZE_HEIGHT - 1) {
                mark[i][j] = MARK_VISITED;
            } else {
                mark[i][j] = MARK_UNVISITED;
            }
        }
    }
 
    print_maze();
   
    find_path();

    return(0);
}

/* FUNCTIONS ************************************/

void exit_error(int error_code, char *extra_info) {
    if (extra_info == NULL) {
        fprintf(stderr, "%s\n", error_msg[error_code]);
    } else {
        fprintf(stderr, "%s: %s\n", error_msg[error_code], extra_info);
    }
    exit(EXIT_FAILURE);
}

void print_maze(void) {
    int i, j;
    for (i = 0; i < MAZE_HEIGHT; ++i) {
        for (j = 0; j < MAZE_HEIGHT; ++j) {
            printf("%s", maze[i][j] == true ? "[]" : "  "); 
        }
        printf("\n");
    }
    return;
}

void find_path(void) {

    return;
}


