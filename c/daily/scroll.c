#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>

#define SCROLL_REPEATS 3

/* TODO make this a circular queue so you can loop it around
 * need a special HEAD marker node
 * make last point to first
 * rewrite loops to stop at HEAD (not null)
 * write function to move the head
 */
// in progress not working

typedef struct Node *Node_ptr;
typedef struct Node {
    bool is_head;
    char data;
    Node_ptr next;
} Node;

typedef struct Queue *Queue_ptr;
typedef struct Queue {
    Node_ptr first;
    Node_ptr last;
} Queue;


void scroll(char *msg, int repeats);

Node_ptr node_create(char c);
Queue_ptr queue_create(void);

Queue_ptr queue_append(Queue_ptr queue, Node_ptr node);
Queue_ptr queue_append_new(Queue_ptr queue, char c);

void node_print(Node_ptr node);
void queue_print(Queue_ptr queue);

void node_destroy(Node_ptr node);
void queue_destroy(Queue_ptr queue);

Queue_ptr queue_create_from_string(char *str);
Queue_ptr queue_advance_head(Queue_ptr queue);

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: scroll \"STRING\"\n");
        exit(EXIT_FAILURE);
    }

    char *msg = argv[1];

    scroll(msg, SCROLL_REPEATS);

    return 0;
}

void scroll(char *msg, int repeats) {
    Queue_ptr queue = queue_create_from_string(msg);

    for (int i = 0; i < repeats; ++i) {
        for (char *c = msg; *c != '\0'; ++c) {
            queue_print(queue);
            printf("\n");
            sleep(1);
            queue = queue_advance_head(queue);
        }
    }

    queue_destroy(queue);
}

Node_ptr node_create(char c) {
    Node_ptr node = malloc(sizeof(Node));
    node->is_head = false;
    node->data = c;
    node->next = NULL;
    return node;
}

Node_ptr node_create_head() {
    Node_ptr node = malloc(sizeof(Node));
    node->is_head = true;
    node->data = 'H';
    node->next = NULL;
    return node;
}

Queue_ptr queue_create(void) {
    Queue_ptr queue = malloc(sizeof(Queue));
    if (!queue) {
        fprintf(stderr, "Problem allocating memory\n");
        exit(EXIT_FAILURE);
    }

    Node_ptr head = node_create_head();
    queue->first = head;
    queue->last = head;
    head->next = queue->first;
    return queue;
}

Node_ptr queue_start(Queue_ptr queue) {
    Node_ptr target = NULL;
    if (queue && queue->first) {
        target = queue->first->next;
    }
    return target;
}

Queue_ptr queue_append(Queue_ptr queue, Node_ptr node) {
    if (queue && node) {
        if (!queue_start(queue)) {
            queue->first->next = node;
        } else {
            queue->last->next = node;
        }
        
        queue->last = node;
        node->next = queue->first;
    }

    return queue;
}

Queue_ptr queue_append_new(Queue_ptr queue, char c) {
    return queue_append(queue, node_create(c));
}

void node_print(Node_ptr node) {
    if (node && node->data && !node->is_head) {
        printf("%c", node->data);
        if (node->next) {
            node_print(node->next);
        }
    }
}

void queue_print(Queue_ptr queue) {
    if (queue && queue->first) {
        node_print(queue->first->next);
    }
}

void node_destroy(Node_ptr node) {
    if (node && !node->is_head) {
        node_destroy(node->next);
        free(node);
    }
}

void queue_destroy(Queue_ptr queue) {
    if (queue) {
        node_destroy(queue->first->next);
        free(queue->first);
        free(queue);
    }
}

Queue_ptr queue_create_from_string(char *str) {
    Queue_ptr queue = queue_create();

    if (queue) {
        for (char *c = str; *c != '\0'; ++c) {
            queue_append_new(queue, *c);
        }
    }
    return queue;
}


Queue_ptr queue_advance_head(Queue_ptr queue) {
    if (queue && queue->first) {

        Node_ptr head = queue->first;
        Node_ptr first = head->next;
        Node_ptr last = queue->last;

        head->next = first->next;
        first->next = head;
        last->next = first;

        queue->first = head;
        queue->last = first;
    }
    return queue;
}
