/* linkedlist.c, Andrew Cashner, 2014-07-28 
 * Simple linked list: Twinkle twinkle
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_ELEMENT 20
#define MAX_TEXT 100
#define LINE1 "Twinkle, twinkle, little star,"
#define LINE2 "How I wonder what you are."
#define LINE3 "Up above the world so high,"
#define LINE4 "Like a diamond in the sky,"

typedef struct node *list_ptr;
typedef struct node {
	char element[MAX_ELEMENT];
	char text[MAX_TEXT];
	list_ptr link;
} node;
void print_xml(list_ptr ptr);
void print_latex(list_ptr ptr);

int main(void)
{
	list_ptr line1, line2, line3, line4;
	line1 = malloc(sizeof(node));
	line2 = malloc(sizeof(node));
	line3 = malloc(sizeof(node));
	line4 = malloc(sizeof(node));
	
	strcpy(line1->element, "l");
	strcpy(line1->text, LINE1);
	strcpy(line2->element, "l");
	strcpy(line2->text, LINE2);
	strcpy(line3->element, "l");
	strcpy(line3->text, LINE3);
	strcpy(line4->element, "l");
	strcpy(line4->text, LINE4);
	
	line4->link = NULL;
	line3->link = line4;
	line2->link = line3;
	line1->link = line2;

	print_xml(line1);
	print_latex(line1);

	return(0);
}

void print_xml(list_ptr ptr)
{
	printf("\n<stanza>\n");
	for (; ptr; ptr = ptr->link) {
		printf("<%s>", ptr->element);
		printf("%s", ptr->text);
		printf("</%s>\n", ptr->element);
	}
	printf("</stanza>\n");
	return;
}

void print_latex(list_ptr ptr)
{
	printf("\n\\begin{tabular}{l}\n");
	for (; ptr; ptr = ptr->link) {
		printf("%s\\\\\n", ptr->text);
	}
	printf("\\end{tabular}\n\n");
	return;
}
