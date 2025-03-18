#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int digits(int);

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: digits [NUMBER]\n");
        exit(EXIT_FAILURE);
    }
    char *input_num_str = argv[1];
    int input_num = atoi(input_num_str);

    printf("%d has %d digits\n", input_num, digits(input_num));

    return 0;
}

int digits(int n) {
    return (int) floor(log10(n)) + 1;
}
