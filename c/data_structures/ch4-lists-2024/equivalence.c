/* Find equivalence relations
 * Given a list of pairs in CSV format (from stdin), output a list of equivalence classes
 *
 * Andrew Cashner, 2024/08/26
 * After Horowitz et al. Program 4.23, p. 169
 *
 * NB - I tried to understand what they were doing by copying while
 * cleaning up and refactoring. In the end I don't really understand 
 * the last algorithm, but it definitely isn't memory safe.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define MAX_CHAR 128
#define MAX_SIZE 80

typedef struct node *node_ptr;
typedef struct node {
    int data;
    node_ptr link;
} node;

FILE *setup_input(int argc, char *argv[]);
int read_text(char buffer[MAX_SIZE][MAX_CHAR], FILE *infile);
node_ptr new_node(int data, node_ptr link);
void register_values(node_ptr values[MAX_SIZE], char text[MAX_SIZE][MAX_CHAR], int line_count);
void free_values(node_ptr values[MAX_SIZE]);
void print_equivalences(node_ptr values[MAX_SIZE], int pair_count);

node_ptr new_stack(int data);
node_ptr push(int data, node_ptr stack);
node_ptr pop(int *data, node_ptr stack);

int main(int argc, char *argv[]) {
    FILE *infile;
    char text[MAX_SIZE][MAX_CHAR] = {{0}};
    int pair_count;
    node_ptr values[MAX_SIZE] = {NULL};
    node_ptr equiv_stack = NULL;

    infile = setup_input(argc, argv);
    pair_count = read_text(text, infile);

    register_values(values, text, pair_count);
    print_equivalences(values, pair_count);

    for (i = 0; i < MAX_SIZE; ++i) {
        equiv_stack = build_equiv_stack(values, printed, i);
    }

    free_values(values);
    
    return 0;
}

FILE *setup_input(int argc, char *argv[]) {
    char *filename;
    FILE *infile;

    if (argc > 2) {
        fprintf(stderr, "Usage: equivalence [- | FILE]\n"
                "With an argument of - read from standard input");
        exit(EXIT_FAILURE);
    } else if (argc == 2) {
        filename = argv[1];
        infile = fopen(filename, "r");
        if (infile == NULL) {
            fprintf(stderr, "Could not open file %s for reading.\n", filename);
            exit(EXIT_FAILURE);
        }
    } else if (argc < 2) {
        infile = stdin;
    }
    return infile;
}

int read_text(char buffer[MAX_SIZE][MAX_CHAR], FILE *infile) {
    int i, line_count;
    char line[MAX_CHAR];

    for (i = 0, line_count = 0; 
            i < MAX_SIZE 
            && fgets(line, sizeof(line), infile) != NULL; 
            ++i) {
        line[strlen(line) - 1] = '\0';
        strcpy(buffer[i], line);
        ++line_count;
    }
    return line_count;
}

node_ptr new_node(int data, node_ptr link) {
    node_ptr node = malloc(sizeof(node));
    if (node == NULL) {
        fprintf(stderr, "Problem allocating memory for node with data %d\n", data);
        exit(EXIT_FAILURE);
    }
    node->data = data;
    node->link = link;
    return node;
}

void register_values(node_ptr values[MAX_SIZE], 
        char text[MAX_SIZE][MAX_CHAR], int line_count) {
    int i;
    int scan_test, first, second;
    char line[MAX_CHAR];

    for (i = 0; i < line_count; ++i) {
        strcpy(line, text[i]);
        scan_test = sscanf(text[i], "%d, %d", &first, &second);
        if (scan_test != 2) {
            fprintf(stderr, "Problem reading input line %d; skipping...\n", i);
            continue;
        }
        values[first] = new_node(second, values[first]);
        values[second] = new_node(first, values[second]);
    }
}

void free_values(node_ptr values[MAX_SIZE]) {
    int i;
    node_ptr current, damned;

    for (i = 0; i < MAX_SIZE; ++i) {
        current = values[i];
        while (current != NULL) {
            damned = current;
            current = current->link;
            free(damned);
        }
    }
}

node_ptr new_stack(int data) {
    return push(data, NULL);
}

node_ptr push(int data, node_ptr stack) {
    node_ptr new_top = new_node(data, stack);
    return new_top;
}

node_ptr pop(int *data, node_ptr stack) {
    node_ptr top;

    if (stack == NULL) {
        fprintf(stderr, "Can't pop from null stack\n");
        exit(EXIT_FAILURE);
    }
    top = stack;
    data = top->data;

    stack = top->link;
    free(top);
    return stack;
}


void print_equivalences(node_ptr values[MAX_SIZE], int pair_count) {
    int i, partner;
    bool printed[MAX_SIZE] = {false};
    node_ptr current, next, top;
    node_ptr partner_stack;

    for (i = 0; i < pair_count; i++) {
        if (!printed[i]) {
            printf("{%d", i);
            printed[i] = true;

            /*****************************/
            /* cleaned up version of original code,
             * don't totally get it,
             * and not sure how to free memory after *
            current = values[i];
            top = NULL;
            for (;;) {
                while (current != NULL) {
                    partner = current->data;
                    if (!printed[partner]) {
                        printf("%5d", j);
                        printed[partner] = true;

                        next = current->link;
                        current->link = top;
                        top = current;
                        current = next;
                    }
                    else {
                        current = current->link;
                    }
                }
                if (top == NULL) break;
                current = values[top->data];
                top = top->link;

                print("}\n");
            }
            *****************************/

            /*****************************/
            /* My attempt at something I understand, but gave up because it
             * starts to recurse. Wouldn't a tree be better? */

            /* Make a stack of unprinted partner values */
            current = values[i];
            partner_stack = new_stack(current->data);

            for (;current != NULL; current = current->link) {
                partner = current->data;
                if (!printed[partner]) {
                    partner_stack = push(current->data, partner_stack);
                    /* mark as printed */
                }
                if (values[partner] != NULL) {
                    /* push unprinted values, recursively */
                }
            }

            /* Print the values from the stack */
            /*****************************/

        }
    }
}




    for (i = 0; i < MAX_SIZE; ++i) {
        current = values[i];
        if (current != NULL && !printed[i]) {
            printf("{%d", i);
            printed[i] = true;

            current = current->link;
            while (current != NULL) {
                current = pop(&partner, current);
                printf(", %d", partner);
            }
            
            printf("}\n");

/*


                current = values[i];
                top = NULL;
                do {
                    while (current != NULL) {
                        partner = current->data;
                        if (!printed[partner])  {
                            printf("%5d", partner);
                            printed[partner] = true;
                            next = current->link;
                            current->link = top;
                            top = current;
                            current = next;
                        } else {
                            current = current->link;
                        }
                    }
                } while (top != NULL);

                current = values[top->data];
*/
        }
    }
}

node_ptr build_equiv_stack(node_ptr partners[MAX_SIZE], 
        bool printed[MAX_SIZE], 
        int match) {
    node_ptr current = partners[match];
    node_ptr stack = NULL;
    int this_data;

    while (current != NULL) {
        this_data = current->data;
        if (!printed[this_data]) {
            stack = push(this_data, stack);
        }
        stack = combine_stacks(
                build_equiv_stack(partners, printed, current->data), stack);
        current = current->link;
    }
    return stack;
}

node_ptr combine_stacks(node_ptr s1, node_ptr s2) {
    node_ptr current = s1;
    while (current->link != NULL) {
        current = current->link;
    }
    current->link = s2;
    return s1;
}
