#include <stdio.h>

int main(void) {
    int data = 0x07;
    int bit_count = 0;
    for (int mask = 0x08; mask > 0; mask = mask >> 1) {
        if ((data & mask) == mask) {
            ++bit_count;
        }
    }
    printf("%d\n", bit_count);
}
