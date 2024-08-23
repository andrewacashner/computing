/* Various operations with a doubly-linked circular linked list with a marked
 * head node 
 * (after Horowitz et al p. 166, ch. 4.5, ex. 2)
 * Andrew Cashner, 2024/08/23
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct node *node_ptr;
typedef struct node {
    bool is_head;
    int data;
    node_ptr prev, next;
} node;

node_ptr new_node(bool is_head, int data, node_ptr prev, node_ptr next);
node_ptr new_list();
node_ptr append(node_ptr head, int new_data);
void assert_not_empty(node_ptr head);
node_ptr first(node_ptr head);
node_ptr last(node_ptr head);
node_ptr find_node(node_ptr head, int value);
node_ptr delete_node(node_ptr head, int value);
node_ptr rotate(node_ptr head, int n);
bool is_last(node_ptr node);
void print_list(node_ptr ls);
void free_list(node_ptr head);

#define MAX_INPUTS 10

int main(void) {
    node_ptr ls = new_list();
    node_ptr match = NULL;
    
    int inputs[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    int i;

    for (i = 0; i < MAX_INPUTS; ++i) {
        ls = append(ls, inputs[i]);
     
    }
    print_list(ls);

    match = find_node(ls, 5);
    if (match) {
        printf("Found match node with data = %d\n", match->data);
    }
    ls = delete_node(ls, 5);
    print_list(ls);

    for (i = 0; i < MAX_INPUTS; i += 2) {
        ls = delete_node(ls, i);
    }
    print_list(ls);

    ls = delete_node(ls, 2); /* Should give error message */

    ls = append(append(append(ls, 10), 20), 30);
    print_list(ls);

    printf("First item %d, last item %d\n", first(ls)->data, last(ls)->data);

    ls = rotate(ls, 1);
    print_list(ls);

    ls = rotate(ls, 5);
    print_list(ls);

    free_list(ls);

    return 0;
}

node_ptr new_node(bool is_head, int data, node_ptr prev, node_ptr next) {
    node_ptr new = malloc(sizeof(node));
    new->is_head = is_head;
    new->data = data;
    new->prev = prev;
    new->next = next;
    return new;
}

node_ptr new_list() {
    node_ptr head = new_node(true, 0, NULL, NULL);
    return head;
}

node_ptr append(node_ptr head, int new_data) {
    node_ptr last, new;
    
    assert_not_empty(head);
    
    if (!head->next && !head->prev) {
        new = new_node(false, new_data, head, head);
        head->next = new;
        head->prev = new;
    } else {
        last = head->prev;
        new = new_node(false, new_data, last, head);
        last->next = new;
        head->prev = new;
    }

    return head;
}

void assert_not_empty(node_ptr ls) {
    if (!ls) {
        fprintf(stderr, "Cannot access empty list\n");
        exit(EXIT_FAILURE);
    }
}
    
node_ptr first(node_ptr head) {
    assert_not_empty(head);
    return head->next;
}

node_ptr last(node_ptr head) {
    assert_not_empty(head);
    return head->prev;
}

node_ptr find_node(node_ptr head, int value) {
    node_ptr current;
    bool found = false;

    assert_not_empty(head);

    for (current = head->next; 
            current && !current->is_head; 
            current = current->next) {

        if (current->data == value) {
            found = true;
            break;
        }
    }
    return (found) ? current : NULL;
}

node_ptr delete_node(node_ptr head, int value) {
    node_ptr match, pre, post;
    
    assert_not_empty(head);

    match = find_node(head, value);
    if (!match) {
        fprintf(stderr, "Cannot find node to delete\n");
    } else {
        pre = match->prev;
        post = match->next;

        pre->next = post;
        post->prev = pre;
        free(match);
    }
    return head;
}

node_ptr connect(node_ptr first, node_ptr second) {
    if (first && second) {
        first->next = second;
        second->prev = first;
    }
    return first;
}

node_ptr rotate(node_ptr head, int n) {
    node_ptr current;
    assert_not_empty(head);

    for (current = head->next; 
            n > 0 && current && !current->is_head; 
            --n, current = current->next) {
        /* Just find the new first element */
    }

    /* Remove the head */
    connect(head->prev, head->next);

    /* Insert head before current element */
    connect(current->prev, head);
    connect(head, current);

    return head;
}

bool is_last(node_ptr node) {
    return !node->next || node->next->is_head;
}


void print_list(node_ptr ls) {
    if (ls) {
        if (ls->is_head) {
            printf("[");
        } else {
            printf("%d%s", ls->data, !is_last(ls) ? ", " : "");
        }

        if (ls->next && !is_last(ls)) {
            print_list(ls->next);
        }

        if (is_last(ls)) {
            printf("]\n");
        }
    }
}

void free_list(node_ptr ls) {
    if (ls) {
        if (ls->next && !is_last(ls)) {
            free_list(ls->next);
        } 
        free(ls);
    }
}

/* Iterative versions */

/*
void print_list(node_ptr head) {
    node_ptr current;
    assert_not_empty(head);
    if (head) {
        printf("[");
        for (current = head->next; current && !current->is_head; current = current->next) {
            printf("%d", current->data);
            if (!is_last(current)) {
                printf(", ");
            }
        }
        printf("]\n");
    }
}
*/

/*
void free_list(node_ptr head) {
    node_ptr current, condemned;
    assert_not_empty(head);
    current = head->next;
    while (current && !current->is_head) {
        condemned = current;
        current = current->next;
        free(condemned);
    }
    free(current);
}
*/



