#include <stdio.h>

#define SPACE 1
#define NOSPACE 0 

int main(void)
{
	int c, status;

	while ( (c = getchar() ) != EOF) {
		if (c == '\n')
			continue;
		if (c == ' ') {
			if (status == SPACE) {
				continue;
			}
			status = SPACE;
		} else {
			status = NOSPACE;
		}
		putchar(c);
	}

	return (0);
}
