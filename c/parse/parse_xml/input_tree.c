/* input_tree.c -- Build a small binary tree by hand
 * Andrew Cashner, 2014-08-30
 * Enter information from standard input, in-order traversal
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX 100 	/* String length */

typedef struct node *tree_ptr;
typedef struct node {
	char data[MAX];
	tree_ptr left_child, right_child;
} node;

/* Function prototypes */
char *input(char data[]);
tree_ptr branch(char contents[]);
void preorder_print(tree_ptr tree);

/* MAIN */
int main(void)
{
	tree_ptr root;
	char data[MAX];

	root = branch(input(data));
	preorder_print(root); 
	printf("\n");
	
	return(0);
}

/* FUNCTIONS */
char *input(char data[])
{
	char line[MAX];

	printf("Enter node data: ");
	fgets(line, sizeof(line), stdin);
	if (line[0] == '\n')
		return(NULL);
	else {
		line[strlen(line) - 1] = '\0';
		strcpy(data, line);
		return(data);
	}
}

tree_ptr branch(char contents[])
{
	tree_ptr tmp;
	tmp = malloc(sizeof(node));

	if (contents != NULL) {
		strcpy(tmp->data, contents);
		tmp->left_child = branch(input(contents));
		tmp->right_child = branch(input(contents));
	}
	return(tmp);
}

void preorder_print(tree_ptr tree)
{
	if (tree != NULL) {
		if (tree->data[0] != '\0') 
			printf("\n<%s>", tree->data);
		preorder_print(tree->left_child);
		if (tree->data[0] != '\0') 
			printf("\n </%s>", tree->data);
		preorder_print(tree->right_child);
	}  
	return;
}

