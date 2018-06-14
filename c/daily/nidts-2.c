/* nidts-2.c -- Reverse stdin from command-line argument
 * Andrew Cashner, 2018/06/13
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[]) {
    int i;
    char *input;
    if (argc == 2) {
        input = argv[1];
        for (i = (strlen(input)); i >= 0; --i) {
            printf("%c", input[i]);
        }
        printf("\n");
    }
    return(0);
}

