/* manualtree.c -- Build a small binary tree by hand
 * Andrew Cashner, 2014-08-30
 * Enter information from standard input, in-order traversal
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX 50 	/* String length */

typedef struct node *tree_ptr;
typedef struct node {
	char data[MAX];
	tree_ptr left_child, right_child;
} node;

/* Function prototypes */
void preorder_print(tree_ptr tree);

/* MAIN */
int main(void)
{
	tree_ptr root, current, left, right;
	char line[100];
	char input[MAX];

	
	/* Build root node: no right child */
	root = malloc(sizeof(node));
	strcpy(root->data, "root");
	root->right_child = NULL;

	/* Create left-child node, link it to root */
	printf("Data for left child of root: ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%s", input);
	current = malloc(sizeof(node));
	strcpy(current->data, input);
	root->left_child = current;

	/* Create next left-child node, link it to current */
	printf("Data for next left child: ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%s", input);
	left = malloc(sizeof(node));
	strcpy(left->data, input);
	current->left_child = left;

	/* Create right-child node, link it to current*/
	printf("Data for right child: ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%s", input);
	right = malloc(sizeof(node));
	strcpy(right->data, input);
	current->right_child = right;

	/* Make null children for lowest branches */
	left->left_child = left->right_child = NULL;
	right->left_child = right->right_child = NULL;

	
	/* Print tree */
	preorder_print(root);

	return(0);
}

void preorder_print(tree_ptr tree)
{
	if (tree) {
		printf("\n");
		printf("%s\n", tree->data);
		preorder_print(tree->left_child);
		preorder_print(tree->right_child);
	}
	return;
}
/*
	printf("\n%s\n", root->data);
	printf("|__/\\\n");
	printf("|__%s\n", current->data);
	printf("  |__%s\n", left->data);
	printf("  |__%s\n", right->data);
	printf("\n");
*/




