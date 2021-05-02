#include <stdio.h>

int main(void){

    char *words[] = { /* array */
        "Ben",
        "Joy",
        "Mama",
        "Daddy",
        "Sancho"
    };

    int word_max = 5;
    int i;

    for (i = 0; i < word_max; ++i) {
        printf("Hello, %s!\n", words[i]);
    }

    return(0);
}
