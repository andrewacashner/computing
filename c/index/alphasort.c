/* alphasort.c -- Andrew A. Cashner, 2017/02/16
 * Alphabetic sorting
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_ALPHA 26
#define MAX_STRING 80
#define MAX_WORDS 120
#define MAX_STACK MAX_WORDS
#define END_ARRAY -999

enum { NO_WORD, WORD } read_state_value;

/* Word lists (stacks) */
typedef struct {
    char text[MAX_STRING];
    int  length;
} word;
word start_order[MAX_WORDS];
word sort_order[MAX_WORDS];
int start_order_top, sort_order_top;

/* Function prototypes */
FILE *setup_input(int argc, char *argv[]);
void copy_words(FILE *infile, word word_list[], int *top);
word create_word(char *new_text);
void push(word new_word, word stack[], int *top);
word pop(word stack[], int *top);
void print_word_list(word word_list[]);
void sort_alpha(word input[], int *input_top,
        word output[], int *output_top);

int main(int argc, char *argv[]) {
    FILE *infile = setup_input(argc, argv);
    start_order_top = sort_order_top = 0;

    copy_words(infile, start_order, &start_order_top);
    print_word_list(start_order);

    sort_alpha(start_order, &start_order_top,
            sort_order, &sort_order_top);
    print_word_list(sort_order);

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

void copy_words(FILE *infile, word word_list[], int *top) {
    char this_word[MAX_STRING];
    word new_word;
    int read_state = NO_WORD;
    int c, i;
    i = 0;
    while ((c = fgetc(infile)) != EOF) {
        if (read_state == WORD) {
            if (c == ' ' || c == '\t' || c == '\n') {
                /* Found end of word */
                this_word[i] = '\0';
                i = 0;
                /* Add word to stack */
                new_word = create_word(this_word);
                push(new_word, start_order, top);
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
    return;
}

word create_word(char *new_text) {
    word this_word;
    strcpy(this_word.text, new_text);
    this_word.length = strlen(new_text);
    return(this_word);
}

void push(word new_word, word stack[], int *top) {
    if (*top < MAX_STACK) {
        stack[*top] = new_word;
    } else {
        fprintf(stderr, "Stack capacity exceeded.\n");
        exit(EXIT_FAILURE);
    }
    ++(*top);
    return;
}

word pop(word stack[], int *top) {
    if (*top == 0) {
        fprintf(stderr, "Empty stack.\n");
        exit(EXIT_FAILURE);
    } else {
        --(*top);
        return(stack[*top]);
    }
}

void print_word_list(word word_list[]) {
    int i;
    for (i = 0; word_list[i].text[0] != '\0' && i < MAX_WORDS; ++i) {
        printf("%d\t%s\n", i, word_list[i].text);
    }
    return;
}
 
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
    
        /* Adjust case */
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

