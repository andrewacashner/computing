#ifndef TREE_H
#define TREE_H

#include "function.h"

#ifdef DEBUG
    #define DEBUG_PRINTF printf
#else
    #define DEBUG_PRINTF(...) {}
#endif // DEBUG

#define TREE_MAX_CHAR_ATOM 20

typedef enum {
    UNKNOWN,
    ROOT,
    FUNCTION,
    NUMBER,
    VARIABLE
} Tree_Node_type;

typedef struct Tree_Node *Tree_Node_ptr;
typedef struct Tree_Node {
    char data[TREE_MAX_CHAR_ATOM];
    Function_sig_ptr function_sig;
    double numeric_value;
    Tree_Node_type type;
    bool is_compiled;
    int level;
    Tree_Node_ptr child;
    Tree_Node_ptr sibling;
    Tree_Node_ptr parent;
} Tree_Node;

Tree_Node_ptr Tree_Node_create(void);
Tree_Node_ptr Tree_Node_create_from_data(char*);
bool Tree_Node_is_root(Tree_Node_ptr);
bool Tree_Node_is_variable(Tree_Node_ptr, char*);
bool Tree_Node_is_function(Tree_Node_ptr node);

Tree_Node_ptr Tree_create(int);
Tree_Node_ptr Tree_add_child(Tree_Node_ptr, Tree_Node_ptr);

void Tree_Node_print(Tree_Node_ptr);

void Tree_destroy(Tree_Node_ptr);

void Tree_Node_debug(Tree_Node_ptr);

int Tree_sibling_count(Tree_Node_ptr);
int Tree_child_count(Tree_Node_ptr);

#endif // TREE_H
