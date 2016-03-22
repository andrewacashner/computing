/* poemtree2.c, Andrew Cashner, 2014-07-28 
 * Representing XML structure as binary tree;
 * printing by preorder traversal
 * Now using function "branch" to create the tree nodes
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ELEMENT_MAX 20
#define DATA_MAX 100
#define LINE1 "Twinkle, twinkle, little star,"
#define LINE2 "How I wonder what you are."
#define LINE3 "Up above the world so high,"
#define LINE4 "Like a diamond in the sky."

typedef struct node *tree_pointer;
typedef struct node {
	char element[ELEMENT_MAX];
	char data[DATA_MAX];
	tree_pointer left_child, right_child;
} node;

void print_tree(tree_pointer ptr);
tree_pointer branch(char element[], char data[], 
				tree_pointer left, tree_pointer right);

int main(void)
{
	tree_pointer poem, stanza1, stanza2, line1, line2, line3, line4;
	line1 =	branch("l", LINE1, NULL, NULL);
	line2 = branch("l", LINE2, NULL, NULL);
	line3 = branch("l", LINE3, NULL, NULL);
	line4 = branch("l", LINE4, NULL, NULL);
	stanza1 = branch("stanza", "", line1, line2); 
	stanza2 = branch("stanza", "", line3, line4);
	poem = branch("poem", "", stanza1, stanza2);
	print_tree(poem);

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
		printf("<%s>\n", ptr->element);
		if (ptr->data)
			printf("%s\n", ptr->data);
		print_tree(ptr->left_child);
		print_tree(ptr->right_child);
		printf("</%s>\n\n", ptr->element);
	}
}

