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

/* FUNCTION compare_nodes
 * Determine which of two given nodes should go first in sorted order
 * Implemented using strcmp on the sort_word of each node
 * RETURN value of strcmp: 0 if equal, <0 if new_node < next_node
 */
int compare_nodes(node_ptr next_node, node_ptr new_node) {
    int strtest = 0;
    
    assert(new_node != NULL && next_node != NULL);
    setlocale(LC_COLLATE, "");
    
    strtest = strcoll(new_node->sort_word, next_node->sort_word);
    DEBUG_PRINT(("compare_nodes new '%s' vs next '%s': result %d\n", 
                new_node->sort_word, next_node->sort_word, strtest));

    return(strtest);
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

/* FUNCTION list_print
 * Print the data from the index linked-list in order, in specified format, to
 * given output file;
 * Recursive function
 * RETURN void
 * TODO If you want to do a table format, the fprintf format width specifier
 * (%-20s) does not work with accented characters; to fix that requires major
 * changes; so let's stick with a simple list
 */
void list_print(FILE *outfile, node_ptr list) {
    if (list != NULL) {
        fprintf(outfile, "| %s %s\n", list->word, list->filename);
        list_print(outfile, list->next);
    }
    return;
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


