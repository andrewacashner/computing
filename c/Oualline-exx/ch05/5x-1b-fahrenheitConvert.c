/* Oualline Ch. 5, programming exercise 1: Celsius to Fahrenheit converter
*  
*  Program asks user for a temperature in Celsius, 
*  captures user response in a character array,
*  scans character array for number and converts it to floating-point variable,
*  converts variable jalue to Fahrenheit,
*  then prints the result.
*
*  The conversion formula is ºF = (9 * ºC) / 5 + 32 .
*/

#include<stdio.h>

char line[10];
	/* Fahrenheit temperature entered by user as character array */ 
float tempC; 	/* Celsius temperature captured from string */
float tempF;	/* Fahrenheit temperature calculated by program */

int main()
{
	printf("\nCELSIUS to FAHRENHEIT CONVERTER\nBy Andrew A. Cashner\n\n");

	printf("Please enter a temperature in degrees Celcius: ");
	fgets(line, sizeof(line), stdin);
	sscanf(line, "%f", &tempC);

	tempF = ( (9 * tempC) / 5 ) + 32;

	printf("%f ºC = %f ºF\n\n", tempC, tempF);

	return (0);
}
