/* Oualline ex. 4-3: Compute area and perimeter of rectangle */
/* Variant with floating variables */

#include<stdio.h>

float rectangleWidth;
float rectangleHeight;
float rectangleArea;
float rectanglePerimeter;

/* Area of rectangle = Width * Height
*  Perimeter = (2 * Width) + (2 * Height) */

int main ()
{

	rectangleWidth = 6.8;
	rectangleHeight = 2.3;
	rectangleArea = rectangleWidth * rectangleHeight;
	rectanglePerimeter = (2 * rectangleWidth) + (2 * rectangleHeight);

	printf("The area of a rectangle %f in wide and %f tall is %f.\n", 
	rectangleWidth, rectangleHeight, rectangleArea);

	printf("The perimeter of the same rectangle is %f.\n", 
	rectanglePerimeter);

	return (0);

}
