/* nidts.c -- Reverse stdin
 * Andrew Cashner, 2018/06/13
 */

#include <stdio.h>

#define MAX_CHARS 1024

int main(void) {
    int c, start;
    char input[MAX_CHARS];

    input[MAX_CHARS] = '\0';
    start = MAX_CHARS;
    
    while ((c = getchar()) != '\n') {
        if (start >= 0) {
            --start;
            input[start] = (char)c; 
        }
    }

    printf("%s\n", &input[start]);

    return(0);
}

