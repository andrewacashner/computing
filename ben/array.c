#include <stdio.h>

int main(void) {
    
    int n;
    int array[] = {55, 43, 789, 1, 46};
    int i;

    for (i = 0; i < 5; ++i) {
        n = array[i];

        printf("%d\n", n);
    }

    return(0);
}
