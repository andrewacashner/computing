#include <stdio.h>
#include <stdlib.h>
#include <math.h>

double geometric(int);
double series(double (*)(int), int, int);

int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Usage: series MIN MAX\n");
        exit(EXIT_FAILURE);
    }

    int min = atoi(argv[1]);
    int max = atoi(argv[2]);
    double (*fn)(int) = geometric;

    double sum = series(fn, min, max);
    printf("%f\n", sum);
    return 0;
}

double geometric(int n) {
    return pow(0.25, n);
}

double series(double (*fn)(int), int min, int max) {
    double sum = 0;
    for (int i = min; i <= max; ++i) {
        sum += fn(i);
    }
    return sum;
}

