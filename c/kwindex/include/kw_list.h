/* kw_list.h -- Andrew A. Cashner, 2017/04/19
 * Linked-list utilities for kwindex
 */

#ifndef KW_LIST_H
#define KW_LIST_H

#include "kw_max_sizes.h"
#include "kw_convert_strings.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>

/* Node for singly-linked list of index data */
typedef struct node *node_ptr;
struct node {
    char word[MAX_STR],         /* Keyword for index */
         sort_word[MAX_STR],    /* Version of word for sort order */
         filename[MAX_LINE];    /* Source of keyword */
    node_ptr next;              /* Address of next node in list */
} node;


/* Create a new node with given data */
extern node_ptr list_create_node(char*, char*);

/* Insert existing new node into list in sorted order */
extern node_ptr list_insert_sorted(node_ptr, char*, char*);

/* Add only select data (filename) from new node to existing node when one field
 * (sort_word) is a duplicate; free the memory for the unused new node  */
extern void list_insert_duplicate(node_ptr, node_ptr);

/* Print the whole list in a given format (recursive) */
extern void list_print(FILE*, node_ptr);

/* Free the memory from the whole list */
extern void list_delete(node_ptr);











#endif /* KW_LIST_H */
