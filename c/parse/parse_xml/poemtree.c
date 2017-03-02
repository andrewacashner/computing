/* poemtree.c, Andrew Cashner, 2014-07-28 
 * Test a data structure for poem in XML
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ELEMENT_MAX 20
#define DATA_MAX 100
#define LINE1 "Twinkle, twinkle, little star,"
#define LINE2 "How I wonder what you are."

typedef struct node *tree_pointer;
typedef struct node {
	char element[ELEMENT_MAX];
	char data[DATA_MAX];
	tree_pointer left_child, right_child;
} node;

void preorder(tree_pointer ptr);

int main(void)
{
	tree_pointer stanza, line1, line2;
	stanza = malloc(sizeof(node));
	line1 = malloc(sizeof(node));
	line2 = malloc(sizeof(node));

	strcpy(stanza->element, "stanza");
	strcpy(line1->element, "l");
	strcpy(line2->element, "l");
	strcpy(line1->data, LINE1);
	strcpy(line2->data, LINE2);

	line1->left_child = line1->right_child = NULL;
	line2->left_child = line2->right_child = NULL;
	stanza->left_child = line1;
	stanza->right_child = line2;

	preorder(stanza);

	return(0);
}

void preorder(tree_pointer ptr) 
/* preorder tree traversal */
{
	if (ptr) {
		printf("\n");
		printf("<%s>", ptr->element);
		if (ptr->data)
			printf("%s", ptr->data);
		preorder(ptr->left_child);
		preorder(ptr->right_child);
		printf("</%s>", ptr->element);
		printf("\n");
	}
		
}

