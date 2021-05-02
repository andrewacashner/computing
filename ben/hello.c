#include <stdio.h>

int main(void){

    int number = 17;

    /*
    while (1) {
        printf("Hello, world!\n");
        number = number - 1;
        if (number == 0) break;
    }
    */

    while (number > 0) {
        printf("Hello, world!\n");
        number--;
    }

    for (number = 17; number > 0; number--) {
        printf("Hello, world!\n");
    }
    
    return(0);
}
