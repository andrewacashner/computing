/* assuming valid ASCII input */
#include <stdio.h>
#include <stdbool.h>

#define MAX_CHAR 128
#define BOOL_STR(x) (x ? "true" : "false")

void register_chars(int *inventory, int increment, char *str);
bool is_permutation(char *stringA, char *stringB);
/* void print_char_table(int *counts); */
void print_test(char *stringA, char *stringB);


int main(void) {
    const int test_value_max = 4;
    char *test_values[][2] = { 
        { "abba", "baab" },
        { "dad", "add" },
        { "bacon", "eggs" },
        { "shut", "tush" } 
    };
    int i;
    for (i = 0; i < test_value_max; ++i) {
        print_test(test_values[i][0], test_values[i][1]);
    }
    return(0);
}

void register_chars(int *inventory, int increment, char *str) {
    int i;
    int c;
    for (i = 0; str[i] != '\0'; ++i) {
        c = str[i];
        inventory[(int)c] += increment;
    }
}

/* (Debug)
void print_char_table(int *counts) {
    int i;
    for (i = 33; i < MAX_CHAR; ++i) {
        printf("%c[%d] ", (char)i, counts[i]);
        if (i % 8 == 0) {
            printf("\n");
        }
    }
    printf("\n");
}
*/

bool is_permutation(char *stringA, char *stringB) {
    bool matches;
    int inventory[MAX_CHAR];
    int i;
    
    for (i = 0; i < MAX_CHAR; ++i) {
        inventory[i] = 0;
    }

    register_chars(inventory, 1, stringA);
    register_chars(inventory, -1, stringB);

    for (matches = true, i = 0; matches == true && i < MAX_CHAR; ++i) {
        matches = inventory[i] == 0;
    }
    return matches;
}

void print_test(char *stringA, char *stringB) {
    bool answer = is_permutation(stringA, stringB);
    printf("%s vs %s: %s\n", stringA, stringB, BOOL_STR(answer));
}
