/* downcase.c -- Convert text file to all lowercase ASCII */

#include <stdio.h>
#include <ctype.h>
#include <locale.h>

int main(void) {

    int c;
    locale_t loc;
    setlocale(LC_ALL, "");
    loc = uselocale((locale_t) 0);
    while ((c = getchar()) != EOF) {
        putchar(tolower_l(c, duplocale(loc)));
    }

    return(0);
}
