#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: nibble NUM\n");
        exit(EXIT_FAILURE);
    }

    uint32_t num = atoi(argv[1]);
    printf("%b\n", num);

    uint32_t nibble_mask = 3; // 0b11

    uint32_t nibbles[16] = { 0 };

    for (uint8_t i = 0; i < 15; ++i) {
        nibbles[i] = (num >> (i * 2)) & nibble_mask;
        printf("%2b %2d\n", nibbles[i], nibbles[i]);
    }

    return 0;
}
