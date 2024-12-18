#include <stdio.h>

int main(void) {
    int sum = 0;
    // tens 0 - 9
    for (int i = 0; i < 10; ++i) {
        // ones 0 - 9
        for (int j = 0; j < 10; ++j) {
            sum += i + j;
        }
    }
    ++sum; // 100
    printf("%d\n", sum);
}


