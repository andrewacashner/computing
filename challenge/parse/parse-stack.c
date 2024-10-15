/* Read tokens of an expression into a stack
 * Andrew Cashner, 2024/10/15 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_CHAR_INPUT 80
#define MAX_CHAR_ATOM 20

const char *WHITESPACE = "  \t\n"; // including non-breaking space ' '

typedef struct Node *Node_ptr;
typedef struct Node {
    char data[MAX_CHAR_ATOM];
    Node_ptr next;
} Node;

typedef struct Stack *Stack_ptr;
typedef struct Stack {
    Node_ptr top;
} Stack;

void read_input(char*, int, FILE*);

Node_ptr Node_create(char*);
Stack_ptr Stack_create(void);
Stack_ptr Stack_create_from_words(char*);
Stack_ptr Stack_push(Stack_ptr, Node_ptr);
Stack_ptr Stack_push_new(Stack_ptr, char *);
Node_ptr Stack_pop(Stack_ptr);
void Node_destroy(Node_ptr);
void Stack_destroy(Stack_ptr);

int Stack_length(Stack_ptr);
void Stack_print(Stack_ptr);

int main(void) {

    char line[MAX_CHAR_INPUT];
    read_input(line, MAX_CHAR_INPUT, stdin);
    printf("%s\n", line);

    Stack_ptr expression_stack = Stack_create_from_words(line);

    printf("Found %d tokens\n", Stack_length(expression_stack));
    printf("Current stack: ");
    Stack_print(expression_stack);

    Node_ptr top = Stack_pop(expression_stack);
    printf("Top was %s\n", top->data);
    Node_destroy(top);

    printf("Current stack: ");
    Stack_print(expression_stack);

    Stack_destroy(expression_stack);

    return 0;
}

void read_input(char* buffer, int max_char, FILE *infile) {
    fgets(buffer, sizeof(char) * max_char, infile);
    buffer[strlen(buffer) - 1] = '\0';
}

Node_ptr Node_create(char *new_data) {
    Node_ptr new_node = malloc(sizeof(Node));
    strcpy(new_node->data, new_data);
    new_node->next = NULL;
    return new_node;
}

Stack_ptr Stack_create(void) {
    Stack_ptr new_stack = malloc(sizeof(Stack));
    if (!new_stack) {
        fprintf(stderr, "Problem allocating memory\n");
        exit(EXIT_FAILURE);
    } else {
        new_stack->top = NULL;
    } 
    return new_stack;
}

Stack_ptr Stack_create_from_words(char *input) {
    Stack_ptr new_stack = Stack_create();

    for (char *next_token = strtok(input, WHITESPACE);
            next_token != NULL;
            next_token = strtok(NULL, WHITESPACE)) {
        Stack_push_new(new_stack, next_token);
    }
    return new_stack;
}

Stack_ptr Stack_push(Stack_ptr stack, Node_ptr new_node)  {
    if (stack && new_node) {
        new_node->next = stack->top;
        stack->top = new_node;
    }
    return stack;
}

Stack_ptr Stack_push_new(Stack_ptr stack, char *new_data) {
    Node_ptr new_node = Node_create(new_data);
    Stack_push(stack, new_node);
    return stack;
}

Node_ptr Stack_pop(Stack_ptr stack) {
    Node_ptr top = NULL;
    if (stack && stack->top) {
        top = stack->top;
        stack->top = stack->top->next;
        top->next = NULL;
    }
    return top;
}

void Node_destroy(Node_ptr head) {
    if (head) {
        Node_destroy(head->next);
        free(head);
    }
}

void Stack_destroy(Stack_ptr stack) {
    if (stack) {
        Node_destroy(stack->top);
        free(stack);
    }
}

int Stack_length(Stack_ptr stack) {
    int count = 0;
    if (stack && stack->top) {
        for (Node_ptr node = stack->top;
                node; 
                node = node->next) {
            ++count;
        }
    }
    return count;
}

void Node_print(Node_ptr node) {
    if (node) {
        printf("%s", node->data);
        if (node->next) {
            printf(" ");
        }
        Node_print(node->next);
    }
}

void Stack_print(Stack_ptr stack) {
    if (stack) {
        Node_ptr node = stack->top;
        Node_print(node);
    }
    printf("\n");
}
