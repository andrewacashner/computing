/* parsepoem.c -- Andrew Cashner -- 2014-08-09
 * 	v0 readstates.c	Read in or out of tags and text
 *	v1 readpoem.c 	Read, store, and print the tags and text of XML file
 *	This version	Uses general function to read, store, and print tags
 */

/* This repeatedly stores each new open or close tag, or text in the same char
 * array, thus it looks like there is empty unmarked up text between <poem> and
 * <stanza>.
 * For the next stage, need to really store all the tags so that they can be
 * compared and a tree built.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DATA_MAX 100
#define OUT 0
#define IN 1
#define CLOSE 0
#define OPEN 1

/* Node structure for binary tree */
typedef struct node *tree_ptr;
typedef struct node {
	char element[DATA_MAX];
	tree_ptr left, right;
} node;

/* Function prototypes */
char *stringbetween(FILE *infile, int start_token, int end_token);

tree_ptr branch(char element[], tree_ptr left, tree_ptr right);
void preorder_print(tree_ptr ptr);

int tag_type;

/***************************************/
int main(int argc, char **argv)
{
	FILE *infile;
	char *text_ptr;
	extern int tag_type;
	int prev_tag_type;

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
	while ((text_ptr = stringbetween(infile, '<', '>')) != NULL) {
		if (tag_type == CLOSE) /* Prev tag can be open or close */
			fprintf(stdout, "End element: ");
		else if (tag_type == OPEN) {
			/* Next open after closing */
			if (prev_tag_type == CLOSE)  
				fprintf(stdout, "Right sibling: ");
			else if (prev_tag_type == OPEN)
				fprintf(stdout, "Left child: ");
		}
		fprintf(stdout, "%s\n", text_ptr); 
		prev_tag_type = tag_type;
	}
	
	/* Store contents in binary tree */

	/* Print contents in specified format */

	return(0);
}

/************************************************************************/
/* Store contents in binary tree 
left child = next tag before end tag
right sib = next tag after end tag

so find next tag
is it end tag?
	if so, find next tag
	that tag is right sib
	
if not, then this tag is left child

void print
if tree_ptr ptr is not null
if ptr-> element is not empty, print
recursive print (ptr->left)
recursive print (ptr->right)
return
*/
/****************************************/



char *stringbetween(FILE *infile, int start_token, int end_token)
{	
	extern int tag_type;
	int c;
	int state = OUT;
	int found = 0;
	char string[DATA_MAX];
	char *string_ptr = string;
	
	tag_type = OPEN;
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
			} else if (c == '/') {
				tag_type = CLOSE;
				continue;
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

tree_ptr branch(char element[], tree_ptr left, tree_ptr right)
{
	/* Return binary tree: 
	 *  Left subtree is for children,
	 *  Right subtree is for siblings,
	 *  Root node contains element name
	 */

	 tree_ptr tmp;
	 tmp = malloc(sizeof(node));
	 strcpy(tmp->element, element);
	 tmp->left = left;
	 tmp->right = right;
	 return (tmp);
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


