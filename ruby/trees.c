/* C version of trees.rb ruby program, 2014-05-12 */

#include<stdio.h>
#include<string.h>

#define MAX 5

void prints(char string[]);

int main(void) 
{
	char species[MAX][10] =
		{{"oak"}, {"maple"}, {"cherry"}, {"sycamore"}, {"dogwood"}};
	int i;

	printf("You listed: \n");

	for (i = 0; i < MAX;  ++i) {
		prints(&species[i][0]);
		printf(" tree\n");
	}
	
	printf("\n");

	for (i = 0; i < MAX; ++i) {
		prints(&species[i][0]);
		if (i < MAX - 1)
			printf(", ");
		else printf("---these are the trees.\n");
	}

	return (0);
}

void prints(char string[]) {
	int i;
	for (i = 0; i < strlen(string); ++i)
		printf("%c", string[i]);
	return;
}



