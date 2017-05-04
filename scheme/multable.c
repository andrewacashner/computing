#include <stdio.h>
#include <stdlib.h>
void multable(int, int, int);

int main(int argc, char *argv[]) {
    int base, max;
    if (argc != 3) {
        exit(EXIT_FAILURE);
    }
    base = atoi(argv[1]);
    max = atoi(argv[2]);
    
    multable(1, base, max);
    return(0);
}
void multable(int multiplier, int base, int max) {
    if (multiplier <= max) {
        printf("%3d\t%3d\n", multiplier, multiplier * base);
        multable(++multiplier, base, max);
    }
    return;
}
