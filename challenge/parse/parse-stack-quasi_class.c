/* Read tokens of an expression into a stack
 * Andrew Cashner, 2024/10/15 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_CHAR_INPUT 80
#define MAX_CHAR_ATOM 20

const char *WHITESPACE = "  \t\n"; // including non-breaking space ' '

typedef struct node *node_ptr;
typedef struct node {
    char data[MAX_CHAR_ATOM];
    node_ptr next;
} node;

typedef struct stack *stack_ptr;
typedef struct stack {
    node_ptr top;

    int (*length)(stack_ptr this);
    void (*print)(stack_ptr this);
} stack;

void read_input(char*, int, FILE*);

node_ptr node_create(char*);
stack_ptr stack_create(void);
stack_ptr stack_create_from_words(char*);
stack_ptr stack_push(stack_ptr, node_ptr);
stack_ptr stack_push_new(stack_ptr, char *);
node_ptr stack_pop(stack_ptr);
void node_destroy(node_ptr);
void stack_destroy(stack_ptr);

int stack_length(stack_ptr);
void stack_print(stack_ptr);

int main(void) {

    char line[MAX_CHAR_INPUT];
    read_input(line, MAX_CHAR_INPUT, stdin);
    printf("%s\n", line);

    stack_ptr expression_stack = stack_create_from_words(line);

    printf("Found %d tokens\n", expression_stack->length(expression_stack));

    expression_stack->print(expression_stack);

    stack_destroy(expression_stack);

    return 0;
}

void read_input(char* buffer, int max_char, FILE *infile) {
    fgets(buffer, sizeof(char) * max_char, infile);
    buffer[strlen(buffer) - 1] = '\0';
}

node_ptr node_create(char *new_data) {
    node_ptr new_node = malloc(sizeof(node));
    strcpy(new_node->data, new_data);
    new_node->next = NULL;
    return new_node;
}

stack_ptr stack_create(void) {
    stack_ptr new_stack = malloc(sizeof(stack));
    if (!new_stack) {
        fprintf(stderr, "Problem allocating memory\n");
        exit(EXIT_FAILURE);
    } else {
        new_stack->top = NULL;
        new_stack->length = stack_length;
        new_stack->print = stack_print;
    } 
    return new_stack;
}

stack_ptr stack_create_from_words(char *input) {
    stack_ptr new_stack = stack_create();

    for (char *next_token = strtok(input, WHITESPACE);
            next_token != NULL;
            next_token = strtok(NULL, WHITESPACE)) {
        stack_push_new(new_stack, next_token);
    }
    return new_stack;
}

stack_ptr stack_push(stack_ptr this_stack, node_ptr new_node)  {
    if (this_stack && new_node) {
        new_node->next = this_stack->top;
        this_stack->top = new_node;
    }
    return this_stack;
}

stack_ptr stack_push_new(stack_ptr this_stack, char *new_data) {
    node_ptr new_node = node_create(new_data);
    stack_push(this_stack, new_node);
    return this_stack;
}

node_ptr stack_pop(stack_ptr this_stack) {
    node_ptr top = NULL;
    if (this_stack && this_stack->top) {
        top = this_stack->top;
        this_stack->top = this_stack->top->next;
        top->next = NULL;
    }
    return top;
}

void node_destroy(node_ptr head) {
    if (head) {
        node_destroy(head->next);
        free(head);
    }
}

void stack_destroy(stack_ptr this_stack) {
    if (this_stack) {
        node_destroy(this_stack->top);
        free(this_stack);
    }
}

int stack_length(stack_ptr this_stack) {
    int count = 0;
    if (this_stack && this_stack->top) {
        for (node_ptr this_node = this_stack->top;
                this_node; 
                this_node = this_node->next) {
            ++count;
        }
    }
    return count;
}

void node_print(node_ptr this_node) {
    if (this_node) {
        printf("%s", this_node->data);
        if (this_node->next) {
            printf(" ");
        }
        node_print(this_node->next);
    }
}

void stack_print(stack_ptr this_stack) {
    if (this_stack) {
        node_ptr this_node = this_stack->top;
        node_print(this_node);
    }
    printf("\n");
}
