/* Parse input into a binary tree
 * Andrew Cashner, 2024/10/15
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_CHAR_INPUT 80
#define MAX_CHAR_ATOM 20

typedef struct Node *Node_ptr;
typedef struct Node {
    char data[MAX_CHAR_ATOM];
    Node_ptr child;
    Node_ptr sibling;
} Node;


void read_input(char*, int, FILE*);
Node_ptr Node_create(void);
Node_ptr Node_create_from_data(char*);
Node_ptr Tree_connect(Node_ptr, Node_ptr, Node_ptr);
Node_ptr Tree_deepest_child(Node_ptr);
void Tree_print_inorder(Node_ptr);
void Tree_delete(Node_ptr);

int main(void) {
//    char line[MAX_CHAR_INPUT];
//    read_input(line, MAX_CHAR_INPUT, stdin);
//    printf("%s\n", line);

    Node_ptr left = Node_create_from_data("left");
    Node_ptr right = Node_create_from_data("right");
    Node_ptr tree = Node_create();
    tree = Tree_connect(tree, left, right);

    Node_ptr branch = Node_create();
    branch = Tree_connect(branch,
            Node_create_from_data("left 2"), 
            Node_create_from_data("right 2"));

    Tree_deepest_child(tree)->child = branch;

    // TODO print hierarchy
    // FN: add to last child
    // FN: add to last sibling
    
    Tree_print_inorder(tree);
    Tree_delete(tree);
    return 0;
}

void read_input(char* buffer, int max_char, FILE *infile) {
    fgets(buffer, sizeof(char) * max_char, infile);
    buffer[strlen(buffer) - 1] = '\0';
}

Node_ptr Node_create(void) {
    Node_ptr new_node = malloc(sizeof(Node));
    strcpy(new_node->data, "");
    new_node->child = NULL;
    new_node->sibling = NULL;
    return new_node;
}

Node_ptr Node_create_from_data(char *data) {
    Node_ptr new_node = Node_create();
    strcpy(new_node->data, data);
    return new_node;
}

Node_ptr Tree_connect(Node_ptr tree, Node_ptr child, Node_ptr sibling) {
    if (tree && !tree->child && !tree->sibling) {
        tree->child = child;
        tree->sibling = sibling;
    }
    return tree;
}

Node_ptr Tree_deepest_child(Node_ptr root) {
    Node_ptr this = root;
    if (this && this->child) {
        this = Tree_deepest_child(this->child);
    }
    return this;
}

Node_ptr Tree_deepest_sibling(Node_ptr root) {
    Node_ptr this = root;
    if (this && this->sibling) {
        this = Tree_deepest_sibling(this->sibling);
    }
    return this;
}

void Node_print(Node_ptr node) {
    if (node) {
        printf("%s", node->data);
    }
}

void Tree_print_inorder(Node_ptr root) {
    Node_ptr this = root;
    if (this) {
        Node_print(this);
        printf("    ");
        Tree_print_inorder(this->child);
        printf("\n    ");
        Tree_print_inorder(this->sibling);
        printf("\n");
    }
}

void Tree_delete(Node_ptr tree) {
    if (tree) {
        Tree_delete(tree->child);
        Tree_delete(tree->sibling);
        free(tree);
    }
}


