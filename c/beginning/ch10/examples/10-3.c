/* Oualline example 10-3 */

#define BIG_NUMBER 10 * 10

int main() {

	/* index for our calculations */
	int index;

	index = 0;

	/* syntax error on next line */
	while (index < BIG_NUMBER) {
		index = index * 8;
	}
	return(0);
}
