#include <stdio.h>

#define MAX 4

int main(void) {
    int map[][MAX] = 
        {   
            {1, 1, 1, 1},
            {1, 1, 0, 1},
            {1, 0, 1, 1},
            {1, 1, 1, 1}
        };
    int x, y;

    for (x = 0; x < MAX; ++x) {
        for (y = 0; y < MAX; ++y) {
            if (map[x][y] == 1) {
                printf("X ");
            } else {
                printf("  ");
            }
        }
        printf("\n");
    }
    return(0);
}
                
