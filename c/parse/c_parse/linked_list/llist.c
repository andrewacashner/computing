/* Linked list */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_TEXT 100

typedef struct node *list_ptr;
typedef struct node { 
	int label;
	char english[MAX_TEXT];
	char spanish[MAX_TEXT];
	list_ptr link;
} node;
enum { HAND, FOOT, HEAD, MOUTH, EYE, EAR, NOSE } words;

list_ptr new_list_item(int label, char english[], char spanish[]);
void add_to_list(list_ptr listname, list_ptr new_item);
char *spanish(list_ptr dictionary, int label);

int main(void)
{
	list_ptr dictionary = new_list_item(HAND, "hand", "mano");
	add_to_list(dictionary, new_list_item(FOOT, "foot", "pie"));
	add_to_list(dictionary, new_list_item(HEAD, "head", "cabeza"));

	printf("Spanish for foot is %s\n", spanish(dictionary, FOOT));
	printf("Spanish for hand is %s\n", spanish(dictionary, HAND));
	printf("Spanish for head is %s\n", spanish(dictionary, HEAD));
	return(0);
}




list_ptr new_list_item(int label, char english[], char spanish[])
{
	list_ptr new_item = malloc(sizeof(node));
	new_item->label = label;
	strcpy(new_item->english, english);
	strcpy(new_item->spanish, spanish);
	new_item->link = NULL;
	return (new_item);
}

void add_to_list(list_ptr listname, list_ptr new_item)
{
	while (listname->link != NULL) {
		listname = listname->link;
	}
	listname->link = new_item;
	return;
}

char *spanish(list_ptr dictionary, int label)
{
	while (dictionary->label != label) {
		dictionary = dictionary->link;
	}
	return(dictionary->spanish);
}
