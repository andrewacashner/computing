/* List number of chars in each word and number of words in input */
#include <stdio.h>
#include <ctype.h>
#include <stdbool.h>

typedef enum { SPACE, WORD } read_state;

bool isspace_tf(int c);

int main(void) {
    int c, chars, words;
    read_state r;

    c = chars = words = 0;
    r = SPACE;
    while (c != EOF) {
        c = getchar();

        if (r == SPACE) {
            if (isspace_tf(c) == false) {
                r = WORD;
                ++chars;
            } 
        } else if (r == WORD) {
            if (c == EOF || isspace_tf(c) == true) {
                r = SPACE;
                ++words;
                printf("%d ", chars);
                chars = 0;
            } else {
                ++chars;
            }
        }
    }
    printf("\n%d\n", words);

    return(0);
}

bool isspace_tf(int c) {
    bool test;

    if (isspace(c) == 0) {
        test = false;
    } else test = true;

    return(test);
}


