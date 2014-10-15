/* Oualline 5, exercise 5-3: Print the perimeter of a rectangle given its height
 * and width. 
 * Asks for user to input the height and length of a rectangle.
 * Gets input as string, then converts strings to numbers.
 * Computes perimeter using formula: perimeter = 2 * (width + height) .
 * Prints the result.
 */

#include<stdio.h>

char line[10]; 	/* Values entered by user */
float height;	/* Rectangle height, entered by user */
float length;	/* Rectangle width, entered by user */
float perimeter; /* Perimeter, calculated */

int main()
{
	printf("\nPERIMETER CALCULATOR by Andrew Cashner\n\n");

	printf("Enter the height of your rectangle: ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%f", &height);

	printf("Enter the length of your rectangle: ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%f", &length);

	perimeter = 2 * (height + length);


	printf("\nRESULT: The perimeter of the rectangle is %f.\n\n", perimeter);

	return(0);
}
