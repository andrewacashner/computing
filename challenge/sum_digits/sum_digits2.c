#include <stdio.h>

int sum_digits();

int main(void) {
    int sum = sum_digits();
    printf("%d\n", sum);
}

int sum_digits() {
    int sum = 0;
    for (int i = 0; i < 10; ++i) {
        sum += 5 * (9 + 2 * i);
    }
    ++sum; // 100
    return sum;
}

