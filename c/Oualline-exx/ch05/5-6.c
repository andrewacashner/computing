/* Oualline ex. 5-6: Ask for first and last name, combine into new variable
 * and print the output */

 #include <stdio.h>
 #include <string.h>

 char first[100];	/* First name of person */
 char last[100];	/* Last name */
 char full[200];	/* First and last name (computed) */

 int main()
 {
 	printf("Enter first name: ");
	fgets(first, sizeof(first), stdin);
	/* trim off last character */
	first[strlen(first)-1] = '\0';

	printf("Enter last name: ");
	fgets(last, sizeof(last), stdin);
	/* trim off last character */
	last[strlen(last)-1] = '\0';

	strcpy(full, first);
	strcat(full, " ");
	strcat(full, last);

	printf("The name is %s\n", full);
	return (0);
}
