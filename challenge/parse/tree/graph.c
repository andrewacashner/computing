/* Graphing calculator
 * Andrew Cashner
 *
 * # CHANGELOG
 * 2024/10/18 - Begun
 * 2024/10/23 - Successful parsing of input expression to a tree and
 *              evaluating it for a range of inputs
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "include/queue.h"
#include "include/tree.h"
#include "include/function.h"

#define MAX_CHAR_INPUT 80

typedef struct Coord {
    double x;
    double y;
} Coord;

void read_input(char*, int, FILE*);
Queue_ptr Queue_tokens_from_string(char*);
Tree_Node_ptr Tree_create_from_tokens(Queue_ptr);
Tree_Node_ptr Tree_compile(Tree_Node_ptr);
Tree_Node_ptr Tree_substitute_var(Tree_Node_ptr, char*, double);
double Tree_evaluate_for(Tree_Node_ptr, char *, double);
double Tree_evaluate(Tree_Node_ptr);
void populate_range_values(Coord[], Tree_Node_ptr, char*, 
        double, double, double, int);
void Coord_array_print(Coord[], int);

const char *USAGE = 
    "Usage: graph VARIABLE_NAME \"(EXPRESSION)\" MIN MAX INCREMENT\n";

int main(int argc, char *argv[]) {
    // TODO or receive input in form (lambda (x) (expr)) ?
    if (argc < 6) {
        fprintf(stderr, USAGE);
        exit(EXIT_FAILURE);
    }
    char *variable_name = argv[1];
    char *expression = argv[2];
    double min = strtof(argv[3], NULL);
    double max = strtof(argv[4], NULL);
    double increment = strtof(argv[5], NULL);

    DEBUG_PRINTF("Input: (lambda (%s) %s)\n", variable_name, expression);
    DEBUG_PRINTF("Evaluate from %f to %f at interval of %f\n", min, max, increment);

    // Parse input into queue of tokens
    // TODO Validate input expression (including matching parens, variable)
    // TODO Validate min, max, increment
    Queue_ptr tokens = Queue_tokens_from_string(expression);

    // Parse token queue into binary tree of s-expressions
    Tree_Node_ptr input_tree = Tree_create_from_tokens(tokens);
#ifdef DEBUG
    Tree_Node_print(input_tree); 
    printf("\n");
#endif

    // Precompile tree (set values for functions and constants except for
    // unknown variable) 
    // TODO specify variable NOT to compile so we don't have multiple
    // uncompiled vars 
    input_tree = Tree_compile(input_tree);
#ifdef DEBUG
    Tree_Node_print(input_tree);
#endif

    // Populate matrix with (x, y) pairs in a given domain
    int iterations = (int)((max - min) / increment);
    DEBUG_PRINTF("Calculating %d iterations\n", iterations);

    Coord values[iterations];
    populate_range_values(values, input_tree, variable_name, 
            min, max, increment, iterations);
    Coord_array_print(values, iterations);

    // TODO Create a bitmap from the matrix data
    // TODO (Maybe) display the bitmap
   
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

// TODO break off into subfunctions? e.g., Tree_Node_compile
// TODO what to do when bad number of arguments found? (can't just return
// NULL, because we need to keep the reference to the tree to free it later)
//
Tree_Node_ptr Tree_compile(Tree_Node_ptr tree) {
    if (tree) {
        if (tree->is_root) {
            if (tree->child) {
                Function_sig_ptr fn_match = 
                    lookup_function(tree->child->data);

                if (fn_match) {
                    tree->child->function_sig = fn_match;
                    DEBUG_PRINTF("Found function \"%s\"\n", fn_match->name);
                    tree->child->is_compiled = true;

                    int siblings = Tree_sibling_count(tree->child);
                    DEBUG_PRINTF("Found %d arguments to \"%s\"\n",
                            siblings, fn_match->name);
                
                    if (!Function_sig_is_variadic(fn_match) &&
                            !Function_sig_matches_args(fn_match, siblings)) {

                        fprintf(stderr, "Function \"%s\" requires %d arguments, but %d provided\n", 
                                fn_match->name, fn_match->required_args, siblings);
                    }

                } else {
                    DEBUG_PRINTF("No match found for symbol \"%s\"\n", tree->child->data);
                }
                tree->is_compiled = true;
            }
        } else if (!tree->is_compiled) {
            char *post_convert_ptr = tree->data;
            double numeric_value = strtof(tree->data, &post_convert_ptr);

            if (post_convert_ptr != tree->data) {
                DEBUG_PRINTF("Found number %f\n", numeric_value);
                tree->numeric_value = numeric_value;
                tree->is_compiled = true;
            } else {
                DEBUG_PRINTF("Found non-number %s\n", tree->data);
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

// Substitute a given numeric value for a given variable in the tree.
Tree_Node_ptr Tree_substitute_var(Tree_Node_ptr tree,
        char *variable_name, double variable_value) {

    if (tree) {
        if (strcmp(tree->data, variable_name) == 0) {
            tree->numeric_value = variable_value;
            DEBUG_PRINTF("Substituted value %f for %s at level %d\n", 
                    variable_value, variable_name, tree->level);
        }
        tree->child = Tree_substitute_var(tree->child, 
                variable_name, variable_value);
        tree->sibling = Tree_substitute_var(tree->sibling, 
                variable_name, variable_value);
    }
    return tree;
}


// TODO evaluate tree substituting for uncompiled values
double Tree_evaluate_for(Tree_Node_ptr tree, char *variable_name, 
        double variable_value) {

    tree = Tree_substitute_var(tree, variable_name, variable_value);
#ifdef DEBUG
    DEBUG_PRINTF("Evaluate tree for %s = %f\n", variable_name, variable_value);
    Tree_Node_print(tree);
#endif
    double result = Tree_evaluate(tree);
    return result;
}

// TODO Evaluate fully-compiled tree (first subtrees then main tree)
double Tree_evaluate(Tree_Node_ptr tree) {
    double result = 0;

    if (tree && tree->is_compiled) {
        if (tree->is_root && tree->child && tree->child->function_sig) {

            generic_function_ptr fn = tree->child->function_sig->function;
            int arg_count = Tree_sibling_count(tree->child);

             DEBUG_PRINTF("Found function \"%s\"  at level %d with %d args\n", 
                     tree->child->function_sig->name, tree->level, arg_count);

             if (fn && arg_count > 0) {
                 double args[arg_count];

                 Tree_Node_ptr current = tree->child->sibling;
                 for (int i = 0; i < arg_count && current; ++i) {

                     if (current->is_root) {
                         args[i] = Tree_evaluate(current);
                     } else {
                         args[i] = current->numeric_value;
                     }
                     DEBUG_PRINTF("Evaluated arg %d = %f\n", i, args[i]);

                     current = current->sibling;
                 }

                 result = (*fn)(arg_count, args);
             } else {
                 fprintf(stderr, "Faulty expression tree\n");
             }
        } else {
            result = Tree_evaluate(tree->child);
        }
    }
    return result;
}

void populate_range_values(Coord values[], Tree_Node_ptr tree, 
        char *variable, double min, double max, double increment, 
        int iterations) {

    double x = min;
    for (int i = 0; i < iterations; ++i) {
        values[i].x = x;
        values[i].y = Tree_evaluate_for(tree, variable, x);
        DEBUG_PRINTF("Calculated y = %f for x = %f\n", values[i].y, x);
        x += increment;
    }
}

void Coord_print(Coord coord) {
    printf("(%f . %f)", coord.x, coord.y);
}

void Coord_array_print(Coord coord[], int length) {
    for (int i = 0; i < length; ++i) {
        Coord_print(coord[i]);
        if (i < length - 1) {
            printf(" ");
        }
    }
    printf("\n");
}
