/* poemtree4.c, Andrew Cashner, 2014-07-28 
 * Representing XML structure as binary tree;
 * printing by preorder traversal
 * poemtree2: Used function "branch" to create the tree nodes
 * poemtree3: Used recursive function call to eliminate hard-coded variables 
 * NOW: Fixing order of elements in binary tree (left and right subtrees):
 *  first child goes in left subtree; first sibling goes in right subtree
 *  Only element field put in node instead of data + element
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX 100
#define LINE1 "Twinkle, twinkle, little star,"
#define LINE2 "How I wonder what you are."
#define LINE3 "Up above the world so high,"
#define LINE4 "Like a diamond in the sky."

typedef struct node *tree_pointer;
typedef struct node {
	char element[MAX];
	tree_pointer left_child, right_child;
} node;

void print_tree(tree_pointer ptr);
tree_pointer branch(char element[], tree_pointer left, tree_pointer right);

int main(void)
{
	tree_pointer root;

	root = 	branch("A", 
		  branch("a", 
		    branch("i",
		      NULL, branch("ii", NULL, NULL)),
		    branch("b",
		      branch("iii", NULL, NULL),
		      NULL),
		  branch("B",
		    branch("c",
		      branch("iv",
		        branch(NULL, 
			branch("v", NULL, NULL)),
		      branch("d", 
		        branch("vi",
			  NULL, /* START */
		    
		
		NULL);


	print_tree(xml);

	return(0);
}

tree_pointer branch(char element[], char data[], 
				tree_pointer left, tree_pointer right)
{
/* Return a binary tree whose left subtree is left_child, whose right subtree is
 * right_child, and whose root node contains item "data"
 */	
 	tree_pointer tmp;
	tmp = malloc(sizeof(node));
	strcpy(tmp->element, element);
	strcpy(tmp->data, data);
	tmp->left_child = left;
	tmp->right_child = right;
	return (tmp);
}	


void print_tree(tree_pointer ptr) 
/* Preorder tree traversal */
{
	if (ptr) {
		if (strcmp(ptr->element, ""))
			printf("<%s>\n", ptr->element);
		if (strcmp(ptr->data, ""))
			printf("%s\n", ptr->data);
		print_tree(ptr->left_child);
		print_tree(ptr->right_child);
		if (strcmp(ptr->element, ""))
			printf("</%s>\n", ptr->element);
	}
}

