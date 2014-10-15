/* Oualline Ch. 5, programming exercise 1: 
*  
*  Program asks user for a temperature in Fahrenheit, 
*  captures user response in a character array,
*  scans character array for number and converts it to floating-point variable,
*  converts variable value to Celsius,
*  then prints the result.
*
*  The conversion formula is ºC = ((ºF - 32) * 5 ) / 9 .
*/

#include<stdio.h>

char line[10];
	/* Fahrenheit temperature entered by user as character array */ 
float tempF;	/* Fahrenheit temperature captured from string */
float tempC; 	/* Celsius temperature calculated by program */

int main()
{
	printf("\nFAHRENHEIT TO CELSIUS CONVERTER\nBy Andrew A. Cashner\n\n");

	printf("Please enter a temperature in degrees Fahrenheit: ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%f", &tempF);

	tempC = ((tempF - 32) * 5) / 9;

	printf("%f ºF = %f ºC\n\n", tempF, tempC);

	return (0);
}
