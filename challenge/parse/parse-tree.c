/* Parse input into a binary tree
 * Andrew Cashner, 2024/10/15
 */

#ifdef DEBUG
    #define DEBUG_PRINTF printf
#else
    #define DEBUG_PRINTF(...) {}
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_CHAR_INPUT 80
#define MAX_CHAR_ATOM 20

typedef struct Node *Node_ptr;
typedef struct Node {
    int level;
    char data[MAX_CHAR_ATOM];
    bool is_root;
    Node_ptr child;
    Node_ptr sibling;
    Node_ptr parent;
} Node;


void read_input(char*, int, FILE*);
Node_ptr Node_create(void);
Node_ptr Node_create_from_data(char*);

Node_ptr Tree_create(int);
Node_ptr Tree_create_from_tokens(char*);
Node_ptr Tree_deepest_child(Node_ptr);
Node_ptr Tree_append_sibling(Node_ptr, Node_ptr);
Node_ptr Tree_append_child(Node_ptr, Node_ptr);
void Node_print(Node_ptr);
void Tree_delete(Node_ptr);

int main(void) {
    char line[MAX_CHAR_INPUT];
    read_input(line, MAX_CHAR_INPUT, stdin);
    printf("%s\n", line);

    Node_ptr input_tree = Tree_create_from_tokens(line);

    Node_print(input_tree); 
    printf("\n");
    Tree_delete(input_tree);

    return 0;
}

void read_input(char* buffer, int max_char, FILE *infile) {
    fgets(buffer, sizeof(char) * max_char, infile);
    buffer[strlen(buffer) - 1] = '\0';
}

Node_ptr Node_create(void) {
    Node_ptr new_node = malloc(sizeof(Node));
    strcpy(new_node->data, "");
    new_node->level = -1;
    new_node->is_root = false;
    new_node->child = NULL;
    new_node->sibling = NULL;
    new_node->parent = NULL;
    return new_node;
}

Node_ptr Node_create_from_data(char *data) {
    Node_ptr new_node = Node_create();
    strcpy(new_node->data, data);
    return new_node;
}

Node_ptr Tree_create(int level) {
    Node_ptr tree = Node_create();
    tree->level = level; // TODO REMOVE
    tree->is_root = true;
    return tree;
}

void Node_print(Node_ptr node) {
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
        }
        if (node->child) {
            DEBUG_PRINTF("c");
            Node_print(node->child);
        }
        if (node->is_root && node->parent) {
            printf(")");
        }
        if (node->sibling) {
            printf(" ");
            DEBUG_PRINTF("s");
            Node_print(node->sibling);
        } 
    }
}

void Tree_delete(Node_ptr tree) {
    if (tree) {
        Tree_delete(tree->child);
        Tree_delete(tree->sibling);
        free(tree);
    }
}

void strip_trailing_parens(char *word) {
    while (word[strlen(word) - 1] == ')') {
        word[strlen(word) - 1] = '\0';
    }
}

// Add a child of root:
// First child will be root->child;
// Subsequent children will be a chain of siblings of the first child
Node_ptr Tree_add_child(Node_ptr root, Node_ptr child) {
    if (root && child) {
        if (!root->child) {
            root->child = child;
        } else {
            Node_ptr current = root->child;
            while (current->sibling) {
                current = current->sibling;
            }
            current->sibling = child;
        }
        child->parent = root;
    }
    return root;
}

Node_ptr Tree_create_from_tokens(char *input) {
    int level = 0;

    const char *WHITESPACE = "  \t\n";

    Node_ptr tree = Tree_create(++level);
    
    Node_ptr current = tree;

    for (char *token = strtok(input, WHITESPACE);
            token != NULL;
            token = strtok(NULL, WHITESPACE)) {

        Node_ptr new_node = Node_create();
        
        if (token[0] == '(') {
            // Start of new expression:
            // The new node will be the parent of a new subtree
            Node_ptr subtree = Tree_create(++level);

            strcpy(new_node->data, token + 1); // Skip paren char
            Tree_add_child(subtree, new_node);

            Tree_add_child(current, subtree);

            current = subtree;

        } else {
            // Continuation of expression:
            // More siblings of first child (= children of current)
            strcpy(new_node->data, token); 

            Tree_add_child(current, new_node);
            /*
            if (current->is_root && !current->child) {
                current->child = new_node;
                new_node->parent = current; 
                current = new_node;
            } else {
                current->sibling = new_node;
                new_node->parent = current->parent;
                current = new_node;
            }
            //            current = new_node;
            */
            // Don't reset current because subsequent siblings will all
            // descend from the same parent as this one
        } 
        
        // Always check for end paren
        if (new_node->data[strlen(new_node->data) - 1] == ')') {
            strip_trailing_parens(new_node->data);

            // TODO Put close parens back into the input stream
            // if endsWith(close-paren) { token = ")"; }

            // Return to parent node of this chain of siblings
            current = current->parent;
            --level; 
        } 
    }
    return tree;
}

void Node_debug(Node_ptr node) {
    printf("   node text: %s\n", node->data);
    printf("   parent text: %s\n", node->parent->data);
}

