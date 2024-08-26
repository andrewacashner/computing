/* stdio_tree.c -- Build a binary tree with inputs from command line 
 * Andrew Cashner, 2014-08-29
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DATA_MAX 10
#define LEFT_CHILD -1
#define RIGHT_CHILD 1
#define NO_CHILD 0

typedef struct node *tree_ptr;
typedef struct node {
	char data[DATA_MAX];
	tree_ptr left_child, right_child;
} node;

tree_ptr branch(char *data, tree_ptr left_child, tree_ptr right_child);
void preorder_print(tree_ptr ptr);

/************************************************************************/

int main(void)
{
	char line[100];
	char data[DATA_MAX];
	int  c;
	int  child_type;
	tree_ptr root, next_branch;
	
	root = branch(NULL, NULL, NULL);
	next_branch = root;

	while (1) {
		printf("Next is Left or Right child? (L/R): ");
		c = getchar();
		if (c == 'L')
			child_type = LEFT_CHILD;
		else if (c == 'R')
			child_type = RIGHT_CHILD;
		else printf("error\n");
			
		printf("Data point: ");
		fgets(line, sizeof(line), stdin);
		sscanf(line, "%s", data);

		/* Insert values into tree ? */
		
	}
	

	return(0);
}

/************************************************************************/

tree_ptr branch(char *data, tree_ptr left_child, tree_ptr right_child)
{
	tree_ptr tmp;
	tmp = malloc(sizeof(node));
	tmp->data = data;
	tmp->left_child = left_child;
	tmp->right_child = right_child;
	return(tmp);
}

/*******************/

void preorder_print(tree_ptr ptr)
{
	if (ptr) {
		if (ptr->data = '\0')
			printf("\n");
		preorder_print(ptr->left_child);
		preorder_print(ptr->right_child);
	}
	return;
}
