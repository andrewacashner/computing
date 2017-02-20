/* listsort.c -- Andrew A. Cashner, 2017/02/19
 * Alphabetic sorting with lists
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_ALPHA 26
#define MAX_STRING 80

enum { NO_WORD, WORD } read_state_value;

/* Word lists (lists) */
typedef struct node *node_ptr;
typedef struct node {
    char text[MAX_STRING];
    node_ptr next;
} node;

/* Function prototypes */
FILE *setup_input(int argc, char *argv[]);
node_ptr copy_words(FILE *infile, node_ptr list);
node_ptr create_node(char *new_text);
node_ptr list_append(node_ptr new_node, node_ptr list);
void print_list(node_ptr list);

/*
node_ptr pop(node_ptr stack, int *top);
void sort_alpha(word input[], int *input_top,
        word output[], int *output_top);
*/

int main(int argc, char *argv[]) {
    FILE *infile = setup_input(argc, argv);
    node_ptr input_list = NULL; 

    input_list = copy_words(infile, input_list);
    print_list(input_list);

/*  sort_alpha(start_order, &start_order_top,
            sort_order, &sort_order_top);
    print_word_list(sort_order);
*/
    return(0);
}

FILE *setup_input(int argc, char *argv[]) {
    FILE *infile;
    char *infile_name;
    if (argc != 2) {
        fprintf(stderr, "Usage: alphasort <input file>\n");
        exit(EXIT_FAILURE);
    }
    infile_name = argv[1];
    infile = fopen(infile_name, "r");
    if (infile == NULL) {
        fprintf(stderr, "Could not open file %s for reading.\n", infile_name);
        exit(EXIT_FAILURE);
    }
    return(infile);
}

node_ptr copy_words(FILE *infile, node_ptr list) {
    node_ptr head = list;
    node_ptr new_node = NULL;
    char this_word[MAX_STRING];
    int read_state = NO_WORD;
    int c, i;

    i = 0;
    while ((c = fgetc(infile)) != EOF) {
        if (read_state == WORD) {
            if (c == ' ' || c == '\t' || c == '\n') {
                /* Found end of word */
                this_word[i] = '\0';
                i = 0;
                /* Add word to list */
                new_node = create_node(this_word);
                head = list_append(new_node, head);
                read_state = NO_WORD;
            } else {
                /* Middle of word */
                this_word[i++] = (char)c;
            } 
        } else if (read_state == NO_WORD) { 
            if (c != ' ' && c!= '\t' && c != '\n') {
                /* Found start of word */
                read_state = WORD;
                this_word[i++] = (char)c;
            }   
        }
    }
    return(list);
}

node_ptr create_node(char *new_text) {
    node_ptr new_node = malloc(sizeof(node_ptr));
    strcpy(new_node->text, new_text);
    new_node->next = NULL;
    return(new_node);
}

node_ptr list_append(node_ptr new_node, node_ptr list) {
    node_ptr head = list;
    int i;

    if (list == NULL) {
        printf("list_append: new list\n");
        head = new_node;
    } else {
        printf("list_append: top of list\n");
        for (i = 0; list->next != NULL; ++i) {
            list = list->next;
            printf("list_append: top found with text %s\n", list->text);
        }
        list->next = new_node;
    }
    return(head);
}

void print_list(node_ptr list) {
    node_ptr tmp = list;
    printf("print_list\n");
    if (tmp != NULL) {
        printf("%s ", tmp->text);
        print_list(tmp->next);
    }
    return;
}

/* node_ptr pop(node_ptr stack, int *top) {
    int i;
    node_ptr this_node = NULL;
    
    if (stack != NULL) {
        for (i = 0; stack->next != NULL && i < *top; ++i) {
            stack = stack->next;
        }
        this_node = stack->next;
        stack->next = NULL;
        --(*top);
    }
    return(this_node);
} 
*/
/* 
void sort_alpha(word input[], int *input_top,
        word output[], int *output_top) {
    
    int alphacount[MAX_ALPHA] = { 0 };
    word alphawords[MAX_ALPHA][MAX_WORDS];
    word this_word;
    int row, col, i, j, k;

    for (i = 0; i < MAX_ALPHA; ++i) {
        for (j = 0; j < MAX_WORDS; ++j) {
            for (k = 0; k < MAX_STRING; ++k) {
                alphawords[i][j].text[k] = '\0';
            }
        }
    }
    
    for (i = *input_top; i > 0; --i) {
        this_word = pop(input, input_top);
        row = this_word.text[0];
    
        if (row >= 'a' && row <= 'z') {
            row -= 'a';
        } else row -= 'A';
        
        ++alphacount[row];
        col = alphacount[row];
        alphawords[row][col] = this_word; 
    }
    for (row = 0; row < MAX_ALPHA; ++row) {
        for (col = 0; col < MAX_WORDS; ++col) {
            if (alphawords[row][col].text[0] != '\0') {
                push(alphawords[row][col], output, output_top); 
            }
        }
    }

    for (row = 0; row < MAX_ALPHA; ++row) {
        if (alphacount[row] > 1 ) {
            for (col = 0; col < alphacount[row]; ++col) {
                this_word = alphawords[row][col];
                if (alphawords[row][col + 1].text[0] != '\0') {
                    for (i = 0; this_word.text[i] != '\0'; ++i) {
                        if (this_word.text[i] >
                                alphawords[row][col + 1].text[i]) {
                            alphawords[row][col] = alphawords[row][col + 1];
                            alphawords[row][col + 1] = this_word;
                            break;
                        }
                    }
                }
                push(alphawords[row][col], output, output_top); 
            }
        }
    }

    return;
}
*/
