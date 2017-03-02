/* dynarray.c -- Andrew Cashner 2014-11-16
 * Dynamically expanding array
 */


#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define MAX 100

int main(void)
{
	char line[MAX], input[MAX];
	char list[MAX] = "\0";
	char *input_ptr;
	int i;

	printf("Enter a word to add to the list. Enter * as last item.\n");
	while (1) {
		fgets(line, sizeof(line), stdin);
		if ((strlen(line) == 2) && (line[0] == '*'))
			break;
		for (i = 0; line[i] != '\n'; ++i)
			input[i] = line[i];
		if (input[0] != '\0')
			input[i] = ' ';
		input_ptr = malloc(sizeof(strlen(line)));
		input_ptr = &list[strlen(list)];
		strcpy(input_ptr, input);
	}
	if (list[0] == '\0') {
		fprintf(stderr, "Empty list.\n");
		return(EXIT_FAILURE);
	} else printf("List:\n%s\n", list);
	
	return(0);
}
		
