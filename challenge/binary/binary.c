#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>

#define MAX_DIGITS 32

void to_binary(int given, char *binary_string);

int main(int argc, char *argv[]) {
    double given;
    char binary_string[MAX_DIGITS];
    const double MAX = pow(2.0, (double)MAX_DIGITS);

    if (argc != 2) {
        fprintf(stderr, "Usage: binary INTEGER\n");
        exit(EXIT_FAILURE);
    }
    
    errno = 0;
    given = strtol(argv[1], NULL, 0);
    if (errno != 0) {
        perror("strtol");
        exit(EXIT_FAILURE);
    }

    if (given >= MAX) {
        fprintf(stderr, "Number too large to compute\n");
        exit(EXIT_FAILURE);
    }

    to_binary(given, binary_string);
    printf("%s\n", binary_string);

    return(0);
}

void to_binary(int given, char *binary_string) {
    int i, j;
    div_t result; 
    int binary_values[MAX_DIGITS];

    i = 0;
    do {
        result = div(given, 2);
        given = result.quot;
        binary_values[i] = result.rem;
        ++i;
    } while (result.quot > 0);

    for (j = i - 1; j >= 0; --j) {
        binary_string[i - j - 1] = binary_values[j] == 1 ? '1' : '0';
    }
    binary_string[i] = '\0';
    return;
}





