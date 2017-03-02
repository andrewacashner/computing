#include <stdio.h>
#define LOWER_UPPER_GAP 32
#define TOTAL_LETTERS 26

void print_alpha(int);

int main(void) 
{
	int iteration;

	for (iteration = 0; iteration < TOTAL_LETTERS; ++iteration) {
		print_alpha(iteration);
	}
	for (iteration = TOTAL_LETTERS - 2; iteration >= 0; --iteration) {
		print_alpha(iteration);
	}
	return(0);
}
void print_alpha(int iteration) 
{
	char letter;
	int position;
	for (letter = 'a', position = 0; letter <= 'z'; ++letter, ++position) {
		if (position == iteration) {
			printf("%c", letter - LOWER_UPPER_GAP);
		} else printf("%c", letter);
	}		
	printf("\n");
	return;
}

