/* sortascending.c -- Andrew A. Cashner, 2015/09/28
 * Sort a list of integers in increasing order
 * Implemented using linked lists.
 */

#include <stdio.h>
#include <stdlib.h>

typedef struct node *node_ptr;
typedef struct node {
  int data;
  node_ptr next;
} node;

node_ptr addnode(node_ptr head, int data);
node_ptr move2head(node_ptr list, int index);
void printlist(node_ptr list);
int minimum_position(node_ptr list);
node_ptr sort_ascending(node_ptr head);

int main(void)
{
  node_ptr list = NULL;
  int i;

  list = addnode(list, 3);
  list = addnode(list, 0);
  list = addnode(list, 1);
  list = addnode(list, 5);
  list = addnode(list, 2);
  printlist(list);
  list = sort_ascending(list);
  printlist(list);
  return(0);
}

node_ptr sort_ascending(node_ptr head)
{
  node_ptr list, sublist;

  if (head == NULL) {
    return(head);
  }
  list = head;

  /* Put the lowest value at the head of the list */
  list = move2head(list, minimum_position(list));
  /* Recursively sort the rest (cdr) of the list and link it to the head */
  sublist = list->next;
  sublist = sort_ascending(sublist);
  list->next = sublist;
  return(list);
}
  
int minimum_position(node_ptr list)
{
  int minimum, min_index, loop_index;

  minimum = list->data;
  min_index = loop_index = 0;
  while (list->next != NULL) {
    if (minimum > list->data) {
      minimum = list->data;
      min_index = loop_index;
    }
    list = list->next;
    ++loop_index;
  }
  return(min_index);
}

node_ptr addnode(node_ptr head, int data)
{
  node_ptr tmp, new;

  /* Create new node with given data */
  new = malloc(sizeof(node_ptr));
  new->data = data;
  new->next = NULL;

  /* Append new node to end of list or set as head of list if list is null */
  if (head == NULL) {
    head = new;
  } else {
    for (tmp = head; tmp->next != NULL; tmp = tmp->next) {
      ; /* Just go to end of list */
    }
    tmp->next = new;
  } 
  return(head);
}

node_ptr move2head(node_ptr head, int index)
{
  node_ptr list, tmp;
  int i;

  /* If there is no list or if the zero index is requested, don't do anything */
  if (head == NULL || index == 0) {
    return(head);
  } 
  /* Find list item just before the one we will move */
  list = head;
  i = 0;
  while (list != NULL && i < index - 1) {
    list = list->next;
    ++i;
  }

  /* Make the next item point to the former head of the list */
  /* Make tmp item skip the next one and point to the one beyond */
  /* Set the new element as the new head */

  tmp = list->next->next;
  list->next->next = head;
  head = list->next;
  list->next = tmp;
  return(head);
}

void printlist(node_ptr head)
{
  node_ptr tmp;
  if (head == NULL) {
    printf("(Empty list)\n");
  } else {
    for (tmp = head; tmp != NULL; tmp = tmp->next) {
      printf("%d ", tmp->data);
    }
    printf("\n");
  }
  return;
}
