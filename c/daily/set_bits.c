// 2025/02/28
#include <stdio.h>
int main(void) {
    unsigned int gpioa = 0;
    unsigned int *GPIOA = &gpioa;
    *GPIOA |= (1 << 31) | (1 << 23) | (1 << 7);
    printf("%x\n", *GPIOA);

    gpioa = 0xFFFFFFFF;
    *GPIOA &= ~((1 << 23) | (1 << 15) | (1 << 7));
    printf("%x\n", *GPIOA);
    return(0);
}
