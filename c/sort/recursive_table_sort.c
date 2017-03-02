/* recursive_table_sort.c -- Andrew A. Cashner, 2016/02/17
 * Alphabetic sorting 
 */

#include <stdio.h>
#include <stdlib.h>

#define MAX_ALPHA 26
#define MAX_WORDS 120

#define MAX_INPUT 6
char *input_list[] = { 
    "two",
    "over",
    "one",
    "three",
    "thou",
    "four"
};

char *alpha_sort(int sort_index, char *input_list[], char *sorted_list[]);
void print_list(char *list[], int length);

int main(void) {
    
    char *sorted_list[MAX_INPUT]; 
    alpha_sort(0, input_list, sorted_list);
    print_list(sorted_list, MAX_INPUT);

    return(0);
}

char *alpha_sort(int sort_index, char *input_list[], char *sorted_list[]) {
    int i, j, k;
    int sort_letter;

    char *alpha_table[MAX_ALPHA][MAX_WORDS];

    if (input_list[0][0] == '\0') {
        return(NULL);
    }
    for (i = 0; i < MAX_ALPHA; ++i) {
        for (j = 0; j < MAX_WORDS; ++j) {
            alpha_table[i][j] = NULL;
        }
    }
    for (i = 0; input_list[i] != NULL; ++i) {
        printf("sorting word '%s'\t", input_list[i]);
        printf("sort_index = %d\n", sort_index);
        sort_letter = input_list[i][sort_index];
        printf("sorting letter '%c' of word '%s'\n", sort_letter, input_list[i]);

        if (sort_letter >= 'a' && sort_letter <= 'z') {
            sort_letter -= 'a';
        } else if (sort_letter >= 'A' && sort_letter <= 'Z') {
            sort_letter -= 'A';
        } else {
            fprintf(stderr, "Unsortable character '%c'\n", sort_letter);
            exit(EXIT_FAILURE);
        }

        for (j = 0; alpha_table[sort_letter][j] != NULL; ++j) {
            ; /* Just count */
        }
        alpha_table[sort_letter][j] = input_list[i];
        if (j > 0) {
            for (k = 0; k < j; ++k) {
                /* XXX does not work */
                printf("subsort: sorting word %s\n", alpha_table[sort_letter][k]);
                alpha_table[sort_letter][k] = alpha_sort(++sort_index, alpha_table[sort_letter], sorted_list);
            }
        }
    }

    k = 0;
    for (i = 0; i < MAX_ALPHA; ++i) {
        for (j = 0; j < MAX_WORDS; ++j) {
            if (alpha_table[i][j] != NULL) {
                sorted_list[k] = alpha_table[i][j];
                ++k;
            }
        }
    }
    return(&sorted_list[0][0]);
}


void print_list(char *list[], int length) {
    int i;
    for (i = 0; i < length; ++i) {
        printf("%s ", list[i]);
    }
    printf("\n");
    return;
}



