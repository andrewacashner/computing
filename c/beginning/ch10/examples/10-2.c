/* Oualline example 10-2 (2013-04-08) */

#define SIZE 20	/* work on 20 elements */

int data[SIZE];	/* some data */
int twice[SIZE]; /* twice some data */

int main() {
	int index; 	/* index into the data */

	for (index = 0; index < SIZE; ++index) {
		data[index] = index;
		twice[index] = index * 2;
	}
	return(0);
}
