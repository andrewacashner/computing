#include <stdio.h>
#define LOWER_UPPER_GAP 32
#define TOTAL_LETTERS 26

int main(void) 
{
	char lower;
	int position;
	int iteration;

	for (iteration = 0; iteration < TOTAL_LETTERS; ++iteration) {
		for (lower = 'a', position = 0; lower <= 'z'; ++lower, ++position) {
			if (position == iteration) {
				printf("%c", lower - LOWER_UPPER_GAP);
			} else printf("%c", lower);
		}
		printf("\n");
	}
	for (iteration = TOTAL_LETTERS - 2; iteration >= 0; --iteration) {
		for (lower = 'a', position = 0; lower <= 'z'; ++lower, ++position) {
			if (position == iteration) {
				printf("%c", lower - LOWER_UPPER_GAP);
			} else printf("%c", lower);
		}
		printf("\n");
	}
	return(0);
}
	
