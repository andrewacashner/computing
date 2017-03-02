/* Oualline, ex. 5-8: Get a number from the user, double it, and print the
 * result. Using fgets plus sscanf (string scanf) to convert text keyboard
 * input (first stored as an array of characters) into numbers (integers)
 * the computer can compute. Avoids deficiencies of the broken
 * scanf function. */

 #include <stdio.h>

 char line[100]; 	/* input line from console */
 int value;			/* a value to double */

 int main()
 {
 	printf("Enter a value: ");

	fgets(line, sizeof(line), stdin);
	sscanf(line, "%d", &value);

	printf("Twice %d is %d\n", value, value * 2);

	return (0);
}
