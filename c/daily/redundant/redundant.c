/* redundant.c
 * Take input list of integers, output list of integers that were repeated in the input list.
 * Andrew A. Cashner, 2015/10/14
 */

#include<stdio.h>
#include<stdlib.h>
#include<stdbool.h>

bool Redundant = false;
typedef struct node *node_ptr;
typedef struct node {
  int data;
  node_ptr next;
} node;

node_ptr insert_sorted(int, node_ptr);
void print_list(node_ptr);

int main(void)
{
  node_ptr inputlist = NULL;
  node_ptr outputlist = NULL;
  char line[100];
  int current_num;
  while (1) {
    fgets(line, sizeof(line), stdin);
    if (line[0] == '\n') break;
    sscanf(line, "%d", &current_num);
    inputlist = insert_sorted(current_num, inputlist);
    if (Redundant == true) {
      outputlist = insert_sorted(current_num, outputlist);
    }
  }
  
  printf("[ ");
  print_list(inputlist);
  printf("] => [ ");
  print_list(outputlist);
  printf("]\n");

  return(0);
}
node_ptr insert_sorted(int newdata, node_ptr head)
{
  node_ptr tmp, new;
  Redundant = false;
  new = malloc(sizeof(node_ptr));
  new->data = newdata;
  new->next = NULL;
  if (head == NULL) {
    return(new);
  } else {
    tmp = head;
  }
  if (newdata < tmp->data) {
    return(new);
  }
  while (tmp->next != NULL) {
    if (newdata == tmp->data) {
      Redundant = true;
      return(head);
    }
    if (newdata < tmp->next->data) {
      new->next = tmp->next;
      tmp->next = new;
      return(head);
    } else {
      tmp = tmp->next;
    }
  }
  tmp->next = new;
  new->next = NULL;
  return(head);
}
 
void print_list(node_ptr head)
{
  if (head != NULL) {
    printf("%d ", head->data);
    print_list(head->next);
  }
  return;
}
  
    
    
