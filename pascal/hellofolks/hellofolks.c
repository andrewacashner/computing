/* Say hello to a group of people */

#include <stdio.h>

const char *names[] = {
      "Joseph"
    , "José"
    , "Josephine"
    , "Giuseppe"
    , "Iosephus"
    , "Josefina"
    , "Josephine"
};

int main(void) {
    int i;
    for (i = 0; names[i] != NULL; ++i) {
        printf("Hello, %s!\n", names[i]);
    }
    return(0);
}
