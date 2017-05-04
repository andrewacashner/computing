#include <stdio.h>
#include <stdlib.h>
int main(int argc, char *argv[]) {
    int i, current, base, max;
    if (argc > 3) {
        exit(EXIT_FAILURE);
    }
    base = atoi(argv[1]);
    max = atoi(argv[2]); 
   
    for (i = 1, current = base; current < max; ++i) {
        printf("x %3d\t=\t%3d\n", i, current);
        current += base;
    }


    return(0);
}
