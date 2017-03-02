/* Oualline example 11-4 */

#include <stdio.h>
int main() {

	short int i;	/* Loop counter */
	unsigned char ch;	/* Loop counter of another kind */

	/* Works */

	for (i = 0x80; i != 0; i = (1 >> 1)) {
		printf("i is %x (%d)\n", i, i);
	}

	/* Only works if ch is UNsigned char */

	for (ch = 0x80; ch != 0; ch = (ch >> 1)) {
		printf("ch is %x (%d)\n", ch, ch);
	}
	return (0);
}
