/* header */
/* Insert existing new node into list in sorted order */
extern node_ptr list_insert_sorted(node_ptr, char*, char*);

/* Add only select data (filename) from new node to existing node when one field
 * (sort_word) is a duplicate; free the memory for the unused new node  */
extern node_ptr list_insert_duplicate(node_ptr, node_ptr);

/* module*/
/* FUNCTION list_insert_sorted
 * Insert a new node in correct sort order into a linked list
 * Create a new linked list if necessary
 * RETURN node_ptr containing address of head of sorted list
 */
node_ptr list_insert_sorted(node_ptr head, char *keyword, char *sourcefile) {
    int strtest = 0;
    bool found = false;
    node_ptr list = NULL,
             new_node = NULL;
             
    /* Don't make a node with empty strings */
    if (strlen(keyword) < 1 || strlen(sourcefile) < 1) {
            return(head);
    } 

    new_node = list_create_node(keyword, sourcefile);

    /* Start new list from new node if head is empty */
    if (head == NULL) {
        return(new_node);
    } 
    strtest = compare_nodes(head, new_node);
    if (strtest == 0) {
        /* If new keyword is already in index, add filename to existing index
         * node and free unused new_node */
        head = list_insert_duplicate(head, new_node);
    } else if (strtest < 0) {
        /* If new_node goes at beginning of list,
         * connect it to list and return new_node as the head */
        new_node->next = head;
        return(new_node);
    } else {
        /* Otherwise find insertion point and insert */
        for (list = head; list->next != NULL; list = list->next) {
            
            strtest = compare_nodes(list, new_node);
            if (strtest == 0) {
                /* Duplicate found */ 
                DEBUG_PRINT(("Duplicate found '%s'\n", new_node->sort_word));
                list = list_insert_duplicate(list, new_node);
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
 * RETURN address of new_node to be freed
 */
node_ptr list_insert_duplicate(node_ptr match_node, node_ptr new_node) {
    char new_filename[MAX_STR] = ", ";

    assert(match_node != NULL && new_node != NULL);
    
    strcat(new_filename, new_node->filename);
    strcat(match_node->filename, new_filename);
    DEBUG_PRINT(("Appending duplicate filename %s\n", new_filename));

    return(match_node);
}


