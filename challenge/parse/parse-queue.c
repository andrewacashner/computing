/* Read tokens of an expression into a queue
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
    Node_ptr prev;
    Node_ptr next;
} Node;

typedef struct Queue *Queue_ptr;
typedef struct Queue {
    Node_ptr first;
    Node_ptr last;
} Queue;

void read_input(char*, int, FILE*);

Node_ptr Node_create(char*);
Queue_ptr Queue_create(void);
Queue_ptr Queue_create_from_words(char*);
Queue_ptr Queue_append(Queue_ptr, Node_ptr);
Queue_ptr Queue_append_new(Queue_ptr, char *);
Node_ptr Queue_pop(Queue_ptr);
void Node_destroy(Node_ptr);
void Queue_destroy(Queue_ptr);

int Queue_length(Queue_ptr);
void Queue_print(Queue_ptr);

int main(void) {

    char line[MAX_CHAR_INPUT];
    read_input(line, MAX_CHAR_INPUT, stdin);
    printf("%s\n", line);

    Queue_ptr expression_queue = Queue_create_from_words(line);

    printf("Found %d tokens\n", Queue_length(expression_queue));
    printf("Current queue: ");
    Queue_print(expression_queue);

    Node_ptr last = Queue_pop(expression_queue);
    printf("Last token was %s\n", last->data);
    Node_destroy(last);
    
    printf("Current queue: ");
    Queue_print(expression_queue);

    Queue_destroy(expression_queue);

    return 0;
}

void read_input(char* buffer, int max_char, FILE *infile) {
    fgets(buffer, sizeof(char) * max_char, infile);
    buffer[strlen(buffer) - 1] = '\0';
}

Node_ptr Node_create(char *new_data) {
    Node_ptr new_node = malloc(sizeof(Node));
    strcpy(new_node->data, new_data);
    new_node->prev = NULL;
    new_node->next = NULL;
    return new_node;
}

Queue_ptr Queue_create(void) {
    Queue_ptr new_queue = malloc(sizeof(Queue));
    if (!new_queue) {
        fprintf(stderr, "Problem allocating memory\n");
        exit(EXIT_FAILURE);
    } else {
        new_queue->first = NULL;
        new_queue->last = NULL;
    } 
    return new_queue;
}

Queue_ptr Queue_create_from_words(char *input) {
    const char *WHITESPACE = "  \t\n"; // including non-breaking space ' '
    
    Queue_ptr new_queue = Queue_create();

    for (char *next_token = strtok(input, WHITESPACE);
            next_token != NULL;
            next_token = strtok(NULL, WHITESPACE)) {
        Queue_append_new(new_queue, next_token);
    }
    return new_queue;
}

Queue_ptr Queue_append(Queue_ptr queue, Node_ptr new_node)  {
    if (queue && new_node) {
        if (!queue->first) {
            new_node->next = NULL;
            new_node->prev = NULL;
            queue->first = new_node;
            queue->last = new_node;
        } else {
            new_node->next = NULL;
            new_node->prev = queue->last;
            queue->last->next = new_node;
            queue->last = new_node;
        }
    }
    return queue;
}

Queue_ptr Queue_append_new(Queue_ptr queue, char *new_data) {
    Node_ptr new_node = Node_create(new_data);
    Queue_append(queue, new_node);
    return queue;
}


Node_ptr Queue_pop(Queue_ptr queue) {
    Node_ptr last_node = NULL;
    if (queue && queue->last) {
        last_node = queue->last;
        queue->last = queue->last->prev;
        queue->last->next = NULL;
    }
    return last_node; 
}


void Node_destroy(Node_ptr head) {
    if (head) {
        Node_destroy(head->next);
        free(head);
    }
}

void Queue_destroy(Queue_ptr queue) {
    if (queue) {
        Node_destroy(queue->first);
        free(queue);
    }
}

int Queue_length(Queue_ptr queue) {
    int count = 0;
    if (queue && queue->first) {
        for (Node_ptr node = queue->first;
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

void Queue_print(Queue_ptr queue) {
    if (queue) {
        Node_ptr node = queue->first;
        Node_print(node);
    }
    printf("\n");
}
