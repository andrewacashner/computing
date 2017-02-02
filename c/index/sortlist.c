/* sortlist.c -- Andrew A. Cashner, 2017/02/02
 *
 * Sort a list of words alphabetically
 *
 * Implemented with linked-list insertion
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_STRING 80*4

typedef struct node *node_ptr;
typedef struct node {
    char word[MAX_STRING];
    node_ptr next;
} node;

node_ptr list_create_item(char *word);
node_ptr list_append(node_ptr list, char *word);
void list_print(node_ptr list);
void list_destroy(node_ptr list);

/* TODO (ignore for now) */
/* node_ptr list_search(node_ptr list, char *word);  */
/* node_ptr list_insert_sorted(node_ptr list, char *word); */

int main(int argc, char **argv) {
    FILE *infile;
    char *infile_name;
    char line[MAX_STRING];
    char word[MAX_STRING];
    node_ptr list = NULL;

    /* Check arguments */
    if (argc != 2) {
        fprintf(stderr, "Usage: sortlist <FILE>\n");
        exit(EXIT_FAILURE);
    }

    /* Open and check input file */ 
    infile_name = argv[1]; 
    infile = fopen(infile_name, "r");
    if (infile == NULL) {
        fprintf(stderr, "Could not open file %s for reading.\n", infile_name);
        exit(EXIT_FAILURE);
    }

    while (fgets(line, sizeof(line), infile) != NULL) {
        sscanf(line, "%s", word);
        list = list_append(list, word); 
    }
    list_print(list);

    /* Clean up and exit */
    list_destroy(list);
    fclose(infile);
    return(0);
}

node_ptr list_create_item(char *word) {
    node_ptr new_node = malloc(sizeof(node_ptr));
    strcpy(new_node->word, word);
    new_node->next = NULL;
    return(new_node);
}

node_ptr list_append(node_ptr head, char *word) {
    node_ptr list;
    node_ptr new_node = list_create_item(word);
    
    if (head == NULL) { /* Create new list if there is none */
        list = new_node;
        return(list);
    } else {
        list = head;
    }
    while (list->next != NULL) {
        list = list->next;
    }
    list->next = new_node;
    return(head);
}

void list_print(node_ptr list) {
    if (list != NULL) {
        printf("%s\n", list->word);
        list_print(list->next);
    }
    return;
}

void list_destroy(node_ptr list) {
    if (list != NULL) {
        list_destroy(list->next);
    }
    free(list);
    return;
}


/* TODO (ignore for now)
* XXX this searches for exact matches only *
node_ptr list_search(node_ptr list, char *word) { 
    node_ptr test = list;
    node_ptr match = NULL;
    while (test->next != NULL) {
        if (strcmp(test->word, word) == 0) {
            printf("Test word %s matches list word %s\n", test->word, word);
            match = test;
            break;
        }
    }
    return(match);
}
            
node_ptr list_insert_sorted(node_ptr list, char *word) {
    node_ptr position = NULL;
    
    node_ptr new_node = malloc(sizeof(node_ptr));
    new_node->word = word;
    new_node->next = NULL;

    position = list_search(list, word);
    if (position != NULL) {
        new_node->next = position;
    }
    return(list);
}
*/

