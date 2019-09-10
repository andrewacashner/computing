/* downcase.c -- Convert uppercase input to lowercase (pass on UTF8) */

#include <stdio.h>
#include <ctype.h>

int main(void) {
    int c;
    while ((c = getchar()) != EOF) {
        putchar(tolower(c));
    }
    return(0);
}
