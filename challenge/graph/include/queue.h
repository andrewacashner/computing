#ifndef QUEUE_H
#define QUEUE_H

#define QUEUE_MAX_CHAR_ATOM 20

typedef struct Queue_Node *Queue_Node_ptr;
typedef struct Queue_Node {
    char data[QUEUE_MAX_CHAR_ATOM];
    Queue_Node_ptr prev;
    Queue_Node_ptr next;
} Queue_Node;

typedef struct Queue *Queue_ptr;
typedef struct Queue {
    Queue_Node_ptr first;
    Queue_Node_ptr last;
} Queue;

Queue_Node_ptr Queue_Node_create(char*);
Queue_ptr Queue_create(void);
Queue_ptr Queue_create_from_words(char*);

Queue_ptr Queue_append(Queue_ptr, Queue_Node_ptr);
Queue_ptr Queue_append_new(Queue_ptr, char *);
Queue_Node_ptr Queue_pop(Queue_ptr);

void Queue_Node_destroy(Queue_Node_ptr);
void Queue_destroy(Queue_ptr);

int Queue_length(Queue_ptr);

void Queue_Node_print(Queue_Node_ptr);
void Queue_print(Queue_ptr);
char *Queue_to_string(Queue_ptr);

#endif // QUEUE.H

