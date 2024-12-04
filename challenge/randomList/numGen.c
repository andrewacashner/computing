#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    int max = (argc > 1) ? atoi(argv[1]) : 100;
    for (int i = 0; i < max; ++i) {
        printf("%d", rand() % max);
        if (i < max - 1) {
            printf(" ");
        }
    }
    printf("\n");
    return 0;
}

