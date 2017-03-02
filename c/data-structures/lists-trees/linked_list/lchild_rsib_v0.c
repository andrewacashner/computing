/* First tree: left child - right sibling tree as linked list 
 * Andrew Cashner, 2014-07-28
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define MAX 100
#define LINE1 "A la jácara, jacarilla,"
#define LINE2 "de buen garbo y lindo porte"

typedef struct node *tree_pointer;
typedef struct node {
	char data[MAX];
	tree_pointer left_child, right_sibling;
} node;

void print_siblings(tree_pointer ptr);

int main(void) 
{

	tree_pointer line1, line2;
	line1 = (tree_pointer)malloc(sizeof(node));
	line2 = (tree_pointer)malloc(sizeof(node));
	
	line1->left_child = NULL;
	line2->left_child = NULL;

	strcpy(line2->data, LINE2);
	line2->right_sibling = NULL;

	strcpy(line1->data, LINE1);
	line1->right_sibling = line2;

	print_siblings(line1);

	return(0);
}

void print_siblings(tree_pointer ptr)
{
	printf("\n");
	for (; ptr; ptr = ptr->right_sibling)
		printf("%s\n", ptr->data);
	printf("\n");
	return;
}
