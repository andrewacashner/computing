/* Binary tree (LCRS)
 * Andrew Cashner, 2024/10/18
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "../include/tree.h"

Tree_Node_ptr Tree_Node_create(void) {
    Tree_Node_ptr new_node = malloc(sizeof(Tree_Node));
    strcpy(new_node->data, "");
    new_node->function_sig = NULL;
    new_node->numeric_value = 0;
    new_node->level = -1;
    new_node->is_root = false;
    new_node->is_compiled = false;
    new_node->child = NULL;
    new_node->sibling = NULL;
    new_node->parent = NULL;
    return new_node;
}

Tree_Node_ptr Tree_Node_create_from_data(char *data) {
    Tree_Node_ptr new_node = Tree_Node_create();
    strcpy(new_node->data, data);
    return new_node;
}

Tree_Node_ptr Tree_create(int level) {
    Tree_Node_ptr tree = Tree_Node_create();
    tree->level = level; 
    tree->is_root = true;
    return tree;
}

// Add a child of root:
// First child will be root->child;
// Subsequent children will be a chain of siblings of the first child
Tree_Node_ptr Tree_add_child(Tree_Node_ptr root, Tree_Node_ptr child) {
    if (root && child) {
        if (!root->child) {
            root->child = child;
        } else {
            Tree_Node_ptr current = root->child;
            while (current->sibling) {
                current = current->sibling;
            }
            current->sibling = child;
        }
        child->parent = root;
    }
    return root;
}

void Tree_Node_print(Tree_Node_ptr node) {
    if (node) {
        if (node->is_root) {
            DEBUG_PRINTF("[%d] ", node->level);
            if (node->parent) {
                if (!node->parent->is_root) {
                    printf(" ");
                }
                printf("(");
            }
        } else {
            printf("%s", node->data);
            if (node->is_compiled) {
                if (node->function_sig != NULL) {
                    DEBUG_PRINTF("  function: %s\n", node->function_sig->name);
                } else {
                    DEBUG_PRINTF("  numeric_value: %f\n", node->numeric_value);
                }
            }
        }
        if (node->child) {
            DEBUG_PRINTF("c");
            Tree_Node_print(node->child);
        }
        if (node->is_root && node->parent) {
            printf(")");
        }
        if (node->sibling) {
            printf(" ");
            DEBUG_PRINTF("s");
            Tree_Node_print(node->sibling);
        } 
    }
}

void Tree_destroy(Tree_Node_ptr tree) {
    if (tree) {
        Tree_destroy(tree->child);
        Tree_destroy(tree->sibling);
        free(tree);
    }
}

void Tree_Node_debug(Tree_Node_ptr node) {
    printf("   node text: %s\n", node->data);
    printf("   parent text: %s\n", node->parent->data);
}

int Tree_sibling_count(Tree_Node_ptr node) {
    int count = 0;
    if (node && node->sibling) {
        for (Tree_Node_ptr current = node->sibling;
                current; 
                current = current->sibling) {
            ++count;
        }
    }
    return count;
}

int Tree_child_count(Tree_Node_ptr node) {
    int count = 0;
    if (node && node->child) {
        // Same as sibling count but including the first child
        count = 1 + Tree_sibling_count(node->child);
    }
    return count;
}

