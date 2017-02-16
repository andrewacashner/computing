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
void group_by_first_letter(word word_list[], int *top,
        word lettergroups[MAX_ALPHA][MAX_WORDS], 
        int sort_keys[MAX_ALPHA][2]);
void sort_alpha_groups(word word_list[], int *top, 
        word lettergroups[MAX_ALPHA][MAX_WORDS], 
        int sort_keys[MAX_ALPHA][2]);


int main(int argc, char *argv[]) {
    FILE *infile = setup_input(argc, argv);
    word lettergroups[MAX_ALPHA][MAX_WORDS];
    int sort_keys[MAX_ALPHA][2];
    
    start_order_top = sort_order_top = 0;

    copy_words(infile, start_order, &start_order_top);
    print_word_list(start_order);

    group_by_first_letter(start_order, &start_order_top, 
            lettergroups, sort_keys);

    sort_alpha_groups(sort_order, &sort_order_top, lettergroups, sort_keys);
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
 
void group_by_first_letter(word word_list[], int *top,
        word lettergroups[MAX_ALPHA][MAX_WORDS], 
        int sort_keys[MAX_ALPHA][2]) {

    word this_word;
    int row, col, i;

    for (i = start_order_top; i > 0; --i) {
        this_word = pop(start_order, &start_order_top);
        row = this_word.text[0];
    
        /* Adjust case */
        if (row >= 'a' && row <= 'z') {
            row -= 'a';
        } else row -= 'A';

        /* Find next empty slot in this alphabetic row */
        for (col = 0; lettergroups[row][col].text[0] != '\0' &&
                col < MAX_WORDS; ++col); /* Just count */
       
        lettergroups[row][col] = this_word;
    }

    for (row = 0; row < MAX_ALPHA; ++row) {
        sort_keys[row][0] = row;
        for (col = 0; lettergroups[row][col].text[0] != '\0' &&
                col < MAX_WORDS; ++col); /* Just count */
        sort_keys[row][1] = col;
    }
    sort_keys[row][0] = END_ARRAY;

    return;
}

void sort_alpha_groups(word word_list[], int *top, 
        word lettergroups[MAX_ALPHA][MAX_WORDS], 
        int sort_keys[MAX_ALPHA][2]) {

    int row, col, i;
    word test;

    for (row = 0; sort_keys[row][0] != END_ARRAY; ++row) {
        for (col = 0; col < sort_keys[row][1]; ++col) {
            test = lettergroups[row][col];
            if (lettergroups[row][col + 1].text[0] != '\0') {
                for (i = 0; test.text[i] != '\0'; ++i) {
                    if (test.text[i] >
                            lettergroups[row][col + 1].text[i]) {
                        lettergroups[row][col] =
                            lettergroups[row][col + 1];
                        lettergroups[row][col + 1] = test;
                        break;
                    }
                }
            }
            push(lettergroups[row][col], word_list, top);
        }
    }
    return;
}


