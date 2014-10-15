/* Oualline exercise 10-1: Write a macro that returns TRUE if its parameter
 * is divisible by 10 and FALSE otherwise. */

 #include<stdio.h>
 
 #define TEN(x) \
 	{if ( ((x) % 10) == 0) {printf("TRUE.\n");} else printf("FALSE.\n");}

 int main() {
	
	char line[100]; 	/* Input buffer */
	int  number;		/* Number input by user */

 	printf("\nEnter an integer to see if it is divisible by 10: ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%d", &number);

	printf("The statement '%d is divisible by 10' is ", number);
/*	if (TEN(number) == 0)
		printf("TRUE.\n");
	else printf("FALSE.\n");
*/
	TEN(number)

	return (0);
}
