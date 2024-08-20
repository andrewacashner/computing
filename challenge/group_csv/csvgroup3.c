/* Given a CSV with two columns (firstname, lastname), merge the two into one
 * and sort the results into two groups. Return a list of both groups,
 * separated by two newlines, to standard output.
 */

/* More declarative
 * 2024/08/19
 */
#include <stdio.h>
#include <stdbool.h>

#define QUOTE '"'
#define COMMA ','
#define SPACE ' '

void writechar(int c) {
    if (c != QUOTE)
        putchar(c);
}

int main(void) {
    int c, newchar;
    bool in_quote = false;

    while ((c = getchar()) != EOF) {
        in_quote = (c == QUOTE) ? !in_quote : in_quote;
        newchar = (c == COMMA && !in_quote) ? SPACE : c;
        writechar(newchar);
    }
    return(0);
}

