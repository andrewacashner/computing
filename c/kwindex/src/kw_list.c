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
    
    strcpy(new_node->word, keyword);

    convert_sort_format(format_sort_word, keyword);
    strcpy(new_node->sort_word, format_sort_word);
    
    strcpy(new_node->filename, sourcefile);
    
    new_node->next = NULL;
  
    return(new_node);
}

/* FUNCTION list_insert_sorted
 * Insert a new node in correct sort order into a linked list
 * Create a new linked list if necessary
 * RETURN node_ptr containing address of head of sorted list
 */
node_ptr list_insert_sorted(node_ptr head, char *keyword, char *sourcefile) {
    int strtest = 0;
    bool found = false;
    node_ptr list = NULL,
             new_node = list_create_node(keyword, sourcefile);

    /* Start new list from new node if head is empty */
    if (head == NULL) {
        return(new_node);
    } 
    strtest = strcmp(new_node->sort_word, head->sort_word);
    if (strtest == 0) {
        /* If new keyword is already in index, add filename to existing index
         * node and free unused new_node */
        list_insert_duplicate(head, new_node);
    } else if (strtest < 0) {
        /* If new_node goes at beginning of list,
         * connect it to list and return new_node as the head */
        new_node->next = head;
        return(new_node);
    } else {
        /* Otherwise find insertion point and insert */
        /* TODO find a way to avoid duplicating code here */
        for (list = head; list->next != NULL; list = list->next) {
            
            strtest = strcmp(new_node->sort_word, list->next->sort_word);
            if (strtest == 0) {
                /* Duplicate found */ 
                list_insert_duplicate(list, new_node);
                break;
            
            } else if (strtest < 0) {
                /* Insertion point found, insert new_node here */
                new_node->next = list->next;
                list->next = new_node;
                found = true;
                break;
            }
        }
        if (found == false) {
            /* No insertion point found, so append to list */
            list->next = new_node;
        }
    }
    return(head);
}

/* FUNCTION list_insert_duplicate
 * When a duplicate keyword is found, we do not need to add a new node to the
 * list; we just need to add a new filename to the filename field of the current
 * node, then we can free the unused new node that was created
 * RETURN void
 */
void list_insert_duplicate(node_ptr match_node, node_ptr new_node) {
    assert(match_node != NULL && new_node != NULL);
  
    strcat(match_node->filename, ", ");
    strcat(match_node->filename, new_node->filename);
    free(new_node); 
    return;
}

/* FUNCTION list_print
 * Print the data from the index linked-list in order, in specified format, to
 * given output file;
 * Recursive function
 * RETURN void
 */
void list_print(FILE *outfile, node_ptr list) {
    if (list != NULL) {
        fprintf(outfile, "%-20s  %s\n", list->word, list->filename);
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


