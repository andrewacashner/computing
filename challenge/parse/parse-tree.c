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
    Node_ptr child;
    Node_ptr sibling;
} Node;


void read_input(char*, int, FILE*);
Node_ptr Node_create(void);
Node_ptr Node_create_from_data(char*);

Node_ptr Tree_create(void);
Node_ptr Tree_create_from_tokens(char*);
Node_ptr Tree_deepest_child(Node_ptr);
Node_ptr Tree_append_sibling(Node_ptr, Node_ptr);
Node_ptr Tree_append_child(Node_ptr, Node_ptr);
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
    Tree_print_inorder(input_tree);
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
    new_node->child = NULL;
    new_node->sibling = NULL;
    return new_node;
}

Node_ptr Node_create_from_data(char *data) {
    Node_ptr new_node = Node_create();
    strcpy(new_node->data, data);
    return new_node;
}

Node_ptr Tree_create(void) {
    Node_ptr tree = Node_create();
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

Node_ptr Tree_append_child(Node_ptr root, Node_ptr child) {
    if (root) {
        Node_ptr last_child = Tree_deepest_child(root);
        last_child->child = child;
    }
    return root;
}

Node_ptr Tree_append_sibling(Node_ptr root, Node_ptr sibling) {
    if (root) {
        Node_ptr last_sibling = Tree_deepest_sibling(root);
        last_sibling->sibling = sibling;
    }
    return root;
}

Node_ptr Tree_append_sibling_to_last_child(Node_ptr root, 
        Node_ptr sibling) {

    if (root) {
        Node_ptr last_child = Tree_deepest_child(root);
        Node_ptr last_sibling = Tree_deepest_sibling(last_child);
        last_sibling->sibling = sibling;
    }
    return root;
}

void Node_print(Node_ptr node) {
    if (node) {
        printf("%s", node->data);
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

// TODO this is not correct lisp output
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

// TODO this is not correct
// Need to account for close parens
// Need to correctly distinguish child, sibling, sibling-of-child
Node_ptr Tree_create_from_tokens(char *input) {
    const char *WHITESPACE = "  \t\n";
    Node_ptr tree = Tree_create();
    Node_ptr prev, current;
   
    strcpy(tree->data, "ROOT");
    current = tree;
    prev = current;

    for (char *token = strtok(input, WHITESPACE);
            token != NULL;
            token = strtok(NULL, WHITESPACE)) {

        if (token[0] == '(') {
            // Create new node from token but skip paren char
            Node_ptr subtree = Node_create_from_data(token + 1); 
            current->child = subtree;
            prev = current;
            current = subtree;

            printf("Found open paren\n");
            printf("   new node text: %s\n", subtree->data);
            printf("   prev text: %s\n", prev->data);
            printf("   current text: %s\n", current->data);


        } else if (token[strlen(token) - 1] == ')') {
            token[strlen(token) - 1] = '\0';
            Node_ptr new_node = Node_create_from_data(token);
            current->sibling = new_node;
            current = prev;

            printf("Found close paren\n");
            printf("   new node text: %s\n", new_node->data);
            printf("   prev text: %s\n", prev->data);
            printf("   current text: %s\n", current->data);
            
        } else {
            Node_ptr new_node = Node_create_from_data(token);
            if (!current->child) {
                current->child = new_node;

                printf("Found new child \n");
                printf("   new node text: %s\n", new_node->data);
                printf("   prev text: %s\n", prev->data);
                printf("   current text: %s\n", current->data);
            } else {
                current = Tree_append_sibling(current, new_node);
                printf("Found another child (=sibling) \n");
                printf("   new node text: %s\n", new_node->data);
                printf("   prev text: %s\n", prev->data);
                printf("   current text: %s\n", current->data);
            }
        }
    }
    return tree;
}
