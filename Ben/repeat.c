/* repeat.c -- Echo back input
 * Ben and Daddy, 2014-11-14
 */

#include <stdio.h>

#define MAX 100

int main(void)
{
	char line[MAX], input[MAX];
	int  repeats, i, n;

	printf("Joy and Mama are playing.\n");
	printf("Tell me something and I will repeat it.\n");
	printf("Type here: ");
	fgets(line, sizeof(line), stdin);
	for (n = 0; line[n] != '\0'; n++)
		input[n] = line[n];
	input[n-1] = '\0';

	printf("How many times should I repeat it?\n");
	printf("Type a number: ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%d", &repeats);

	for (i = repeats; i > 0; i--)
		printf("%s ", input);

	printf("\n");	

	return(0);
}




