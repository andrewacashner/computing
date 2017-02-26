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
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

/* FUNCTION PROTOTYPES *********************************/
void print_maze(void);
void find_path(void);

/* GLOBAL DEFINITIONS AND VARIABLES ********************/

/* The maze is stored as an array of 0s and 1s,
 * with the open spaces as 0s.
 * The outside borders of the maze are all 1.
 * The entrance of the maze is at (x,y) position (1,1);
 * the exit is at (MAZE_HEIGHT - 1, MAZE_WIDTH - 1).
* We will use a random number generator to populate our maze.
 */
#define MAZE_HEIGHT 25
#define MAZE_WIDTH 25
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

/* We will create 'move', an array of 'offset' structs, to store the 
 * moves for each direction.
 * The data for 'move' are in 'offset_value': [0] horizontal, [1] vertical
 */
#define MAX_DIRECTIONS 8
int offset_value[MAX_DIRECTIONS][2] = {
    {-1, 0},    /* N  */
    {-1, -1},   /* NE */
    {0, 1},     /* E  */
    {1, 1},     /* SE */
    {1, 0},     /* SW */
    {1, -1},    /* W  */
    {0, -1},    /* NW */
    {-1, -1}
};
typedef struct {
    short int vert, horiz;
} offset;
offset move[MAX_DIRECTIONS]; 


/* MAIN *************************************************/

int main(void) {
    int i, j;
    /* Populate the maze:
     * The outside rows and columns are MAZE_OCCUPIED (1);
     * the start and end position are MAZE_UNOCCUPIED (0);
     * the rest are randomly generated. 
     * */
    for (i = 0; i < MAZE_HEIGHT; ++i) {
        for (j = 0; j < MAZE_WIDTH; ++j) {

            if (i > 0 && i < MAZE_HEIGHT - 1 &&
                    j > 0 && j < MAZE_WIDTH - 1) {
                /* Inside maze, random 1 or 0 */
                maze[i][j] = rand() % 2; 
            } else { 
                /* Borders = 1 */
                maze[i][j] = MAZE_OCCUPIED;
            }
        }
    }
    /* Start and end positions */
    maze[1][1] = MAZE_EXIT = MAZE_UNOCCUPIED;

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
 
    /* Populate 'move' array with data from 'offset_value' */
    for (i = 0; i < MAX_DIRECTIONS; ++i) {
        move[i].vert  = offset_value[i][0];
        move[i].horiz = offset_value[i][1];
    }

    print_maze();
   
    find_path();

    return(0);
}

/* FUNCTIONS ************************************/

void print_maze(void) {
    int i, j;
    for (i = 0; i < MAZE_HEIGHT; ++i) {
        for (j = 0; j < MAZE_HEIGHT; ++j) {
            printf("%d ", maze[i][j]); 
        }
        printf("\n");
    }
    printf("\n");
    return;
}

void find_path(void) {

    return;
}


