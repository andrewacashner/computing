/* parsepoem.c -- Andrew Cashner -- 2014-08-09
 * 	v0 readstates.c	Read in or out of tags and text
 *	v1 readpoem.c 	Read, store, and print the tags and text of XML file
 *	This version	Uses general function to read, store, and print tags
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DATA_MAX 100
#define OUT 0
#define IN 1
#define CLOSE 0
#define OPEN 1

#define LEFT -1 /* Left child */
#define RIGHT 1 /* Right child = sibling in hierarchy */
#define EMPTY 0 /* No child */

/* Node structure for binary tree */
typedef struct node *tree_ptr;
typedef struct node {
	char element[DATA_MAX];
	tree_ptr left, right;
} node;

/* Function prototypes */
char *get_element(FILE *infile);
tree_ptr insert(tree_ptr current, char *data, int child_type);
void preorder_print(tree_ptr ptr);

/***************************************/
int main(int argc, char **argv)
{
	FILE *infile;
	char *text_ptr;
	int tag_type;
	int prev_tag_type;
	tree_ptr xml_root, xml_node; 

	/* READ COMMAND LINE ARGUMENT TO GET INPUT FILENAME, OPEN IT */
	if (argc != 2) {
		fprintf(stderr, "Incorrect number of arguments.\n");
		fprintf(stderr, "Usage: readpoem <input_file_name>\n");
		exit(8);
	}
	infile = fopen(argv[1], "r");
	if (infile == NULL) {
		fprintf(stderr, "Could not open file %s for reading.\n", argv[1]);
		exit(8);
	}
	
	/* READ INPUT FILE, PARSE FOR STRUCTURE */
	xml_root = xml_node = NULL;
	xml_root = insert(xml_root, "xml", EMPTY);
	xml_node = xml_root;

	while ((text_ptr = get_element(infile)) != NULL) {
		if (text_ptr[0] == '/') {
			tag_type = CLOSE;
			text_ptr++;
		} else tag_type = OPEN;

		if (tag_type == CLOSE) {
			/* Prev tag can be open or close */
			printf("End element: %s\n", text_ptr);
			if (prev_tag_type == CLOSE) {
				/* Closed element after another close means
				 * previous element had no children */
				 xml_node = insert(xml_node, text_ptr, EMPTY);
			} else if (tag_type == OPEN) {
				if (prev_tag_type == OPEN) {
					/* Next open element after another open = child of prev
					 * open element */
					printf("Left child: %s\n", text_ptr);
				 	xml_node = insert(xml_node, text_ptr, LEFT);
				} else if (prev_tag_type == CLOSE) {
					/* Next open element after closing tag of
					 * element= sibling of closed element */
					printf("Right sibling: %s\n", text_ptr);
				 	xml_node = insert(xml_node, text_ptr, RIGHT);
				}
			}
			prev_tag_type = tag_type;
		}
	}

	/* Store contents in binary tree */

	/* Print contents in specified format */
	preorder_print(xml_root);

	return(0);
}

/****************************************/

char *get_element(FILE *infile)
{	
	int c;
	int state = OUT;
	int found = 0;
	int start_token = '<';
	int end_token = '>';
	char string[DATA_MAX];
	char *string_ptr = string;
	
	while ((c = fgetc(infile)) != EOF) {
		if (state == OUT) {
			if (c == start_token) {
				state = IN;
				continue;
			}
		} else if (state == IN) {
			if (c == end_token) {
				++found;
				*string_ptr = '\0';
				string_ptr = string;
				break;
			} else {
				*string_ptr = (char)c;
				++string_ptr;
			}
		}
	}
	if (found > 0) 
		return(string_ptr);
	else return(NULL);
}

/********************/


tree_ptr insert(tree_ptr current, char *data, int child_type)
{
	current = malloc(sizeof(node));

	strcpy(current->element, data);

	if (child_type == LEFT)
		current->left = insert(current, data, LEFT);
	else if (child_type == RIGHT)
		current->right = insert(current, data, RIGHT);
	else if (child_type == EMPTY) 
		current->left = current->right = NULL;

	return(current);
}


void preorder_print(tree_ptr ptr)
{
	/* Preorder tree traversal */

	if (ptr) {
		if (strcmp(ptr->element, ""))
			printf("<%s>\n", ptr->element);
		preorder_print(ptr->left);
		preorder_print(ptr->right);
	}
	return;
}

/****************************************/


