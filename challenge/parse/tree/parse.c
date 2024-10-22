/* Parse input into a binary tree
 * Andrew Cashner, 2024/10/18
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "include/queue.h"
#include "include/tree.h"
#include "include/function.h"

#define MAX_CHAR_INPUT 80

void read_input(char*, int, FILE*);
Queue_ptr Queue_tokens_from_string(char*);
Tree_Node_ptr Tree_create_from_tokens(Queue_ptr);
Tree_Node_ptr Tree_compile(Tree_Node_ptr);

int main(void) {
    // Read expression from standard input
    char line[MAX_CHAR_INPUT];
    read_input(line, MAX_CHAR_INPUT, stdin);
    printf("%s\n", line);

    // Parse input into queue of tokens
    // TODO Validate input (including matching parens)
    Queue_ptr tokens = Queue_tokens_from_string(line);

    // Parse token queue into binary tree of s-expressions
    Tree_Node_ptr input_tree = Tree_create_from_tokens(tokens);

    // Test tree
    Tree_Node_print(input_tree); 
    printf("\n");

    // TODO Evaluate tree (find matching functions) minus variable
    //      - new 'evaluate' function (in function.c) that evaluates a
    //      subtree 
    //      - add a field to the tree nodes for the function signature;
    //      the first time, just set these
    //          - the second time when plugging in x, only lookup the value
    //          if that field is null (i.e., for X values)? 
    //          - (Create a local "closure" lookup table with the value for
    //          X?)
    // TODO For a range of x values, find y and store results in a matrix
    // TODO Create a bitmap from the matrix data
    // TODO (Maybe) display the bitmap
 
    input_tree = Tree_compile(input_tree);

    // Clean up
    Queue_destroy(tokens);
    Tree_destroy(input_tree);

    return 0;
}

void read_input(char* buffer, int max_char, FILE *infile) {
    fgets(buffer, sizeof(char) * max_char, infile);
    buffer[strlen(buffer) - 1] = '\0';
}

void strcpy_filter_parens(char *dest, char *src) {
    int dest_i = 0;
    for (int i = 0; src[i] != '\0'; ++i) {
        if (src[i] != '(' && src[i] != ')') {
            dest[dest_i] = src[i];
            ++dest_i;
        }
    }
    dest[dest_i] = '\0';
}

Queue_ptr Queue_tokens_from_string(char *input) {
    const char *WHITESPACE = "  \t\n";
    
    Queue_ptr new_queue = Queue_create();

    char buffer[MAX_CHAR_INPUT];
    strcpy(buffer, input);

    for (char *token = strtok(buffer, WHITESPACE);
            token;
            token = strtok(NULL, WHITESPACE)) {

        char new_data[QUEUE_MAX_CHAR_ATOM];
        strcpy_filter_parens(new_data, token);

        for (int i = 0; token[i] == '('; ++i) {
            Queue_append_new(new_queue, "(");
        }
     
      Queue_append_new(new_queue, new_data);

        for (int i = strlen(token) - 1; token[i] == ')'; --i) {
            Queue_append_new(new_queue, ")");
        }
    }
    return new_queue;
}

void strip_trailing_parens(char *word) {
    while (word[strlen(word) - 1] == ')') {
        word[strlen(word) - 1] = '\0';
    }
}

bool token_is_open_paren(Queue_Node_ptr token) {
    return strcmp(token->data, "(") == 0;
}

bool token_is_close_paren(Queue_Node_ptr token) {
    return strcmp(token->data, ")") == 0;
}

Tree_Node_ptr Tree_create_from_tokens(Queue_ptr tokens) {
    int level = 0;

    Tree_Node_ptr tree = Tree_create(++level);
    Tree_Node_ptr current = tree;

    for (Queue_Node_ptr token = tokens->first;
            token;
            token = token->next) {

        if (token_is_open_paren(token)) {
            // Start of new expression:
            // The new node will be the parent of a new subtree
            Tree_Node_ptr subtree = Tree_create(++level);
            Tree_add_child(current, subtree);
            current = subtree;

        } else if (token_is_close_paren(token)) {
            // Return to parent node of this chain of siblings
            current = current->parent;
            --level; 

        } else {
            // Continuation of expression:
            // More siblings of first child (= children of current)
            Tree_Node_ptr new_node = Tree_Node_create_from_data(token->data);
            Tree_add_child(current, new_node);
            
            // Don't reset current because subsequent siblings will all
            // descend from the same parent as this one
        } 
    }
    return tree;
}

// TODO check number of siblings to check arguments of found functions
Tree_Node_ptr Tree_compile(Tree_Node_ptr tree) {
    if (tree) {
        if (tree->is_root && tree->child) {
            Function_sig_ptr fn_match = 
                lookup_function(tree->child->data);

            if (fn_match) {
                tree->child->function_sig = fn_match;
                DEBUG_PRINTF("Found function \"%s\"\n", fn_match->name);
            } else {
                DEBUG_PRINTF("No match found for symbol \"%s\"\n", tree->child->data);
            }
        } 
        if (tree->child) {
            tree->child = Tree_compile(tree->child);
        }
        if (tree->sibling) {
            tree->sibling = Tree_compile(tree->sibling);
        }
    }
    return tree;
}
