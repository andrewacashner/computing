/* Parse input into a binary tree
 * Andrew Cashner, 2024/10/15
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_CHAR_INPUT 80
#define MAX_CHAR_ATOM 20

typedef struct Node *Node_ptr;
typedef struct Node {
    char data[MAX_CHAR_ATOM];
    bool is_root;
    int level;
    Node_ptr child;
    Node_ptr sibling;
    Node_ptr parent;
} Node;


void read_input(char*, int, FILE*);
Node_ptr Node_create(void);
Node_ptr Node_create_from_data(char*);

Node_ptr Tree_create(void);
Node_ptr Tree_create_from_tokens(char*);
Node_ptr Tree_deepest_child(Node_ptr);
Node_ptr Tree_append_sibling(Node_ptr, Node_ptr);
Node_ptr Tree_append_child(Node_ptr, Node_ptr);
void Node_print(Node_ptr);
void Tree_print_inorder(Node_ptr);
void Tree_delete(Node_ptr);

int main(void) {
    char line[MAX_CHAR_INPUT];
    read_input(line, MAX_CHAR_INPUT, stdin);
    printf("%s\n", line);
/*
    Node_ptr left = Node_create_from_data("left");
    Node_ptr right = Node_create_from_data("right");
    Node_ptr tree = Tree_create();
    tree = Tree_append_child(tree, left);
    left = Tree_append_sibling(left, right);

    Node_ptr left2 = Node_create_from_data("left2");
    tree = Tree_append_child(tree, left2);
    left2 = Tree_append_sibling(left2, Node_create_from_data("right2"));

    Tree_print_inorder(tree);
    Tree_delete(tree);
*/
    Node_ptr input_tree = Tree_create_from_tokens(line);
//    Tree_print_inorder(input_tree);
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

Node_ptr Tree_create(void) {
    Node_ptr tree = Node_create();
    tree->is_root = true;
    return tree;
}

void Node_print(Node_ptr node) {
    if (node) {
        if (node->is_root) {
            printf(" ("); // TODO no space at very start
        } else {
            printf("%s", node->data);
        }
        if (node->child) {
            Node_print(node->child);
        }
        if (node->sibling) {
            printf(" ");
            Node_print(node->sibling);
        } 
        if (node->is_root) {
            printf(")");
        }
    }
}

void Tree_print_inorder_xml(Node_ptr root, int indent_level) {
    int shiftwidth = 2;
    Node_ptr this = root;
    if (this) {
        if (strlen(this->data) == 0) {
            printf("<root>");
        } else {
            printf("\n%*s<el>", indent_level, "");
        }

        Node_print(this);
       
        if (this->child) {
            Tree_print_inorder_xml(this->child, 
                    indent_level + shiftwidth);
        }

        if (strlen(this->data) == 0) {
            printf("</root>");
        } else {
            printf("</el>");
        }

        if (this->sibling) {
            Tree_print_inorder_xml(this->sibling, indent_level);
        }

        if (!this->child && !this->sibling) {
            printf("\n%*s", indent_level - shiftwidth, "");
        }

    }
}

void Tree_print_inorder_lisp(Node_ptr root) {
    Node_ptr this = root;
    if (this) {
        Node_print(this);
        if (this->child) {
            printf(" c[");
            Tree_print_inorder_lisp(this->child);
        }
        if (this->sibling) {
            printf(" s");
            Tree_print_inorder_lisp(this->sibling);
        } 
        if (this->child) {
            printf("]");
        }
    } 
}

void Tree_print_inorder(Node_ptr root) {
    Tree_print_inorder_lisp(root);
    printf("\n");
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
            printf("Add first child\n");
            root->child = child;
        } else {
            printf("Add additional child (=sibling)\n");
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
    const char *WHITESPACE = "  \t\n";

    Node_ptr tree = Tree_create();
    strcpy(tree->data, "ROOT");
    
    Node_ptr current = tree;

    for (char *token = strtok(input, WHITESPACE);
            token != NULL;
            token = strtok(NULL, WHITESPACE)) {

        Node_ptr new_node = Node_create();
        
        if (token[0] == '(') {
            // Start of new expression:
            // The new node will be the parent of a new subtree
            printf("Found open paren\n");

            Node_ptr subtree = Tree_create();

            strcpy(new_node->data, token + 1); // Skip paren char
            Tree_add_child(subtree, new_node);

            Tree_add_child(current, subtree);

            current = subtree;

            printf("   new node text: %s\n", new_node->data);
            printf("   parent text: %s\n", new_node->parent->data);
            printf("   current text: %s\n", current->data);

        } else {
            printf("Found another child (=sibling) \n");
            
            strcpy(new_node->data, token); 
            Tree_add_child(current->parent, new_node);
            current = new_node;

            printf("   new node text: %s\n", new_node->data);
            printf("   parent text: %s\n", new_node->parent->data);
            printf("   current text: %s\n", current->data);
        } 
        
        // Always check for end paren
        if (new_node->data[strlen(new_node->data) - 1] == ')') {

            // Trim trailing paren
            new_node->data[strlen(new_node->data) - 1] = '\0';

            // Return to parent node of this chain of siblings
            current = current->parent;

            printf("Found close paren\n");
            printf("   parent text: %s\n", new_node->parent->data);
            printf("   current text: %s\n", current->data);
        } 
    }
    return tree;
}
