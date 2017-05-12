/* kw_list.c -- Andrew A. Cashner, 2017/04/19
 * Linked-list utilities for kwindex
 */

#include "kw_list.h"

/* FUNCTION list_create_node
 * Create a new node (allocating necessary memory) for linked list using given
 * data
 * RETURN address of node in node_ptr
 */
node_ptr list_create_node(char *keyword, char *sourcefile) {
    node_ptr new_node = (node_ptr)malloc(sizeof(node));
    char format_sort_word[MAX_STR] = "";

    assert(keyword != NULL && sourcefile != NULL);
    
    strcpy(new_node->word, keyword);

    convert_sort_format(format_sort_word, keyword);
    strcpy(new_node->sort_word, format_sort_word);
    
    strcpy(new_node->filename, sourcefile);
    
    new_node->next = NULL;
  
    return(new_node);
}

/* FUNCTION list_append
 * Add a node to the end of the list
 * RETURN head of list
 */
node_ptr list_append(node_ptr head, node_ptr new_node) {
    node_ptr current = NULL;
    if (head == NULL) {
        return(new_node);
    }
    for (current = head; current->next != NULL; current = current->next) {
        continue;  /* Go to end of list */
    }
    current->next = new_node;
    return(head);
}

/* FUNCTION list_delete
 * Free the memory used for the linked-list index;
 * Using recursive call to access all list elements
 * RETURN void
 */
void list_delete(node_ptr list) {
    if (list != NULL) {
        list_delete(list->next);
    }
    free(list);
    return;
}


