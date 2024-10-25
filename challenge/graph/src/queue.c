/* Queue
 * Andrew Cashner, 2024/10/18
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../include/queue.h"

Queue_Node_ptr Queue_Node_create(char *new_data) {
    Queue_Node_ptr new_node = malloc(sizeof(Queue_Node));
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

Queue_ptr Queue_append(Queue_ptr queue, Queue_Node_ptr new_node)  {
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
    Queue_Node_ptr new_node = Queue_Node_create(new_data);
    Queue_append(queue, new_node);
    return queue;
}


Queue_Node_ptr Queue_pop(Queue_ptr queue) {
    Queue_Node_ptr last_node = NULL;
    if (queue && queue->last) {
        last_node = queue->last;
        queue->last = queue->last->prev;
        queue->last->next = NULL;
    }
    return last_node; 
}


void Queue_Node_destroy(Queue_Node_ptr head) {
    if (head) {
        Queue_Node_destroy(head->next);
        free(head);
    }
}

void Queue_destroy(Queue_ptr queue) {
    if (queue) {
        Queue_Node_destroy(queue->first);
        free(queue);
    }
}

int Queue_length(Queue_ptr queue) {
    int count = 0;
    if (queue && queue->first) {
        for (Queue_Node_ptr node = queue->first;
                node; 
                node = node->next) {
            ++count;
        }
    }
    return count;
}

void Queue_Node_print(Queue_Node_ptr node) {
    if (node) {
        printf("%s", node->data);
        if (node->next) {
            printf(" ");
        }
        Queue_Node_print(node->next);
    }
}

void Queue_print(Queue_ptr queue) {
    if (queue) {
        Queue_Node_ptr node = queue->first;
        Queue_Node_print(node);
    }
    printf("\n");
}

char *Queue_to_string(Queue_ptr queue) {
    int queue_length = Queue_length(queue);

    // Room for the data cell of every queue member + spaces between
    int buffer_length = sizeof(char) * QUEUE_MAX_CHAR_ATOM * queue_length
                        + sizeof(char) * (queue_length - 1);

    char *buffer = malloc(buffer_length);
    strcpy(buffer, "");
    char *buffer_tail = buffer;
    
    if (queue) {
        for (Queue_Node_ptr this_node = queue->first; 
                this_node; 
                this_node = this_node->next) {

            char *maybe_space = this_node->next ? " " : "";
            buffer_tail += sprintf(buffer_tail, "%s%s", 
                    this_node->data, maybe_space);
        }
    }
    return buffer;
}

