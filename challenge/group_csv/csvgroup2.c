/* Given a CSV with two columns (firstname, lastname), merge the two into one
 * and sort the results into two groups. Return a list of both groups,
 * separated by two newlines, to standard output.
 */

#include <stdio.h>
#include <stdbool.h>

int main(void) {
    char c;
    bool in_quote = false;

    while ((c = getchar()) != EOF) {
        if (c == '"') {
            in_quote = !in_quote;
        } else if (c == ',' && !in_quote) {
            putchar(' ');
        } else {
            putchar(c);
        }
    }
    return(0);
}

