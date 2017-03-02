/* min_start_char.c -- Andrew Cashner, 2017/02/02
 * Find the minimum starting char of an array of words
 */

#include <stdio.h>

void print_list(char **list, int list_length);
char min_start_char(char **list, int list_length);
char *first_word(char **list, int list_length);

int main(void) {
    char *list[] = { "donkey", "apple", "cat", "zebra", "pig" };
    int list_length = 5;

    print_list(list, list_length);

    printf("Minimum starting character: %c\n",
            min_start_char(list, list_length));

    printf("First word: %s\n", first_word(list, list_length));

    return(0);
}

void print_list(char **list, int list_length) {
    int i;
    for (i = 0; i < list_length; ++i) {
        printf("%s%s", list[i], i < list_length - 1 ? ", " : "\n");
    }
    return;
}

char min_start_char(char **list, int list_length) {
    char current, next;
    if (list_length > 0) {
        current = *list[0];
        next = *( (list + 1)[0] );
        if (current > next) {
                return(min_start_char(++list, --list_length));
        }
    } 
    return(*list[0]);
}

char *first_word(char **list, int list_length) {
    char current, next;
    if (list_length > 0) {
        current = *list[0];
        next = *( (list + 1)[0] );
        if (current > next) {
                return(first_word(++list, --list_length));
        }
    } 
    return(*list);
}
