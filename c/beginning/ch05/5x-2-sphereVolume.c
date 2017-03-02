/* Oualline exercies 5-2: Calculate volume of a sphere
*  Program will ask user for the radius of a sphere.
*  Based on a declared constant value for pi (3.14159), it will calculate using
*  this formula: V = (4/3) * pi * r^3 .
*/

#include<stdio.h>

const float PI = 3.14159; /* Declares value for pi */
char line[10];			  /* User input */
float radius;			 /* Radius value, scanned from user character input */
float volume;			 /* Volume, calculated */

int main()
{
	printf("\nANDREW CASHNER WILL CALCULATE THE VOLUME OF YOUR SPHERE\n\n");

	printf("Enter the radius of your sphere: ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%f", &radius);

	volume = (4 * PI * radius * radius * radius) / 3;

	printf("The volume of your sphere is %f.\n\n", volume);

	return(0);
}


	
