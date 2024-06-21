#include <stdio.h>
#include <stdlib.h>

int compose(int fn1(), int fn2(), int arg) {
    return(fn2(fn1(arg)));
}

int inc(int n) {
    return(n + 1);
}

int twice(int n) {
    return(n + n);
}

int double_neighbor(int n) {
    return compose(inc, twice, n);
}


int main(int argc, char *argv[]) {
    int n, neighbor;

    if (argc != 2) {
        printf("Usage: compose N\n");
        exit(EXIT_FAILURE);
    }
    
    n = atoi(argv[1]);
    neighbor = double_neighbor(n);

    printf("double neighbor of %d is %d\n", n, neighbor);

    return(0);
}
