/* Oualline ex. 4-3: Compute area and perimeter of rectangle */

#include<stdio.h>

int rectangleWidth;
int rectangleHeight;
int rectangleArea;
int rectanglePerimeter;

/* Area of rectangle = Width * Height
*  Perimeter = (2 * Width) + (2 * Height) */

int main ()
{

	rectangleWidth = 3;
	rectangleHeight = 5;
	rectangleArea = rectangleWidth * rectangleHeight;
	rectanglePerimeter = (2 * rectangleWidth) + (2 * rectangleHeight);

	printf("The area of a rectangle %d in wide and %d tall is %d.\n", 
	rectangleWidth, rectangleHeight, rectangleArea);

	printf("The perimeter of the same rectangle is %d.\n", 
	rectanglePerimeter);

	return (0);

}
